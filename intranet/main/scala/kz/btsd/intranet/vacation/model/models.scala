package kz.btsd.intranet.vacation.model

import java.time.LocalDate

import kz.btsd.intranet.vacation.dao.Employees

object VacationType extends Enumeration {
  type `type` = Value
  val Yearly /*, Emergent, Unpaid*/  = Value
}
object VacationRequestStatus extends Enumeration {
  type status = Value
  val New, Declined, Accepted, Sent, Vacation = Value
}

// модель записи в таблице
case class VacationRequest (
  id: Long,
  userId: Long,
  `type`: VacationType.`type`,
  startDate: LocalDate,
  endDate: LocalDate,
  substitute: String,

  status: VacationRequestStatus.status = VacationRequestStatus.New,
  declineReason: Option[String] // причина отказа
)

// модель пользователя
case class User (
  id: Long,
  email: String
) {
  lazy val employeeOpt = Employees.get(email)
  def employee = employeeOpt.get
}

// модель сотрудника
case class Employee (
  email: String,
  name: String,
  department: String,
  position: String,
  vacationObservers: Set[String] //мейлы, которым будут отправляться уведомления о заявках сотрудника
)