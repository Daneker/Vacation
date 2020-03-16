package kz.btsd.intranet.vacation.dao

import java.io.InputStreamReader
import java.nio.file.{Files, Paths}

import kz.btsd.intranet.vacation.cfg
import kz.btsd.intranet.vacation.model.Employee
import org.apache.commons.csv.{CSVFormat, CSVParser}

import scala.collection._


/** Объект, что парсит список сотрудников и достает информацию о них
  */
object Employees {

  private val employees: Map[String, Employee] = {
    val pathToFile = cfg.getString("employees.csv")
    var employees = mutable.Map[String, Employee]()
    val reader = new InputStreamReader(getClass.getResourceAsStream(pathToFile))
    val csvParser: CSVParser = new CSVParser(reader, CSVFormat.DEFAULT
      .withHeader("email", "name", "department", "position", "vacation_observers")
      .withFirstRecordAsHeader()
      .withTrim()
    )

    try {
      csvParser.forEach { csvRecord =>
        val vacationObservers = csvRecord.get("vacation_observers").split(',').map(_.trim).toSet
        val employee = Employee(
          email = csvRecord.get("email"),
          name = csvRecord.get("name"),
          department = csvRecord.get("department"),
          position = csvRecord.get("position"),
          vacationObservers = vacationObservers)
        employees += (employee.email -> employee)
      }
    } finally {
      if (reader != null) reader.close()
      if (csvParser != null) csvParser.close()
    }

    //валидация csv - получатели уведомлений сами должны быть сотрудниками
    for {
      employee <- employees
      vacationObserver  <- employee._2.vacationObservers
    } {
      if(!employees.contains(vacationObserver)) throw new Exception(s"Получатель уведомлений $vacationObserver не найден в списке сотрудников!")
    }

    employees
  }


  /** Возвращает сотрудника по имейлу
    * @param email имейл пользователя
    */
  def get(email: String): Option[Employee] = employees.get(email)

}