package kz.btsd.intranet.vacation.dao

import java.time.LocalDate

import kz.btsd.intranet.vacation.Postgres._
import kz.btsd.intranet.vacation.model._
import slick.jdbc.{GetResult, PositionedResult, SetParameter}
import slick.jdbc.PostgresProfile.api._
import slick.lifted.Tag

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success, Try}

class OverlappingDatesException extends Exception
class InvalidStatusTransitionException(msg: String) extends Exception(msg)
class VacationRequestNotFoundException extends Exception
class IllegalRequestDeletionException extends Exception

object InvalidStatusTransitionException {
  def unapply(e: Throwable): Option[InvalidStatusTransitionException] =
    if(e.getMessage contains "invalid status transition") Some(new InvalidStatusTransitionException(e.getMessage)) else None
}


object VacationRequestImplicits {
  implicit val typeMapper = MappedColumnType.base[VacationType.`type`, String](e => e.toString, s => VacationType.withName(s))
  implicit val statusMapper = MappedColumnType.base[VacationRequestStatus.status, String](e => e.toString, s => VacationRequestStatus.withName(s))

  implicit val getVacationTypeResult = GetResult(rs => VacationType.withName(rs.nextString()))
  implicit val getVrStatusResult = GetResult(rs => VacationRequestStatus.withName(rs.nextString()))
  implicit val getVrResult = GetResult(r => VacationRequest(r.<<, r.<<, r.<<, r.<<, r.<<, r.<<, r.<<, r.<<))

  implicit val statusSetter = SetParameter[VacationRequestStatus.status] {
    (status, params) => params.setString(status.toString)
  }
}

private class VacationRequests(tag: Tag) extends Table[VacationRequest](tag, "vacation_requests") {
  import VacationRequestImplicits._

  def id = column[Long]("id", O.PrimaryKey)
  def userId = column[Long]("user_id")
  def vacationType = column[VacationType.`type`]("vacation_type")
  def startDate = column[LocalDate]("start_date")
  def endDate = column[LocalDate]("end_date")
  def substitute = column[String]("substitute")

  def status = column[VacationRequestStatus.status]("status")
  def declineReason = column[Option[String]]("decline_reason")

  def * = (id, userId, vacationType, startDate, endDate, substitute, status, declineReason) <> (VacationRequest.tupled, VacationRequest.unapply)
}

/**
  * DAO для работы с заявками
  * Статус заявок меняется согласно с enumeration VacationType.`type` в прямом порядке.
  */
object VacationRequests {

  import VacationRequestImplicits._

  private val requests = TableQuery[VacationRequests]

  /** Создает новую заявку.
    * @throws OverlappingDatesException, если заявка в подобном промежутке уже существует.
    * @throws IllegalArgumentException, если дата окончания < дата начала.
    *
    * @param uid        id пользователя
    * @param vtype      вид отпуска
    * @param startD     дата началы отпуска
    * @param endD       дата окончания отпуска
    * @param substitute замена пользователя во время отпуска
    */
  // todo может ли пользователь создать запрос со старой датой?
  def create(uid: Long, vtype: VacationType.`type`, startD: LocalDate, endD: LocalDate, substitute: String): Try[VacationRequest] = {
    if (endD.isBefore(startD) || endD.isEqual(startD)) Failure(new IllegalArgumentException(s"$endD.isBefore($startD) || $endD.isEqual($startD)"))
    else {
      val insertRequestA = (requests.map(r =>
        (r.userId, r.vacationType, r.startDate, r.endDate, r.substitute, r.status))
        returning requests.map(_.id)) += (uid, vtype, startD, endD, substitute, VacationRequestStatus.New)

      insertRequestA.map(id => VacationRequest(id, uid, vtype, startD, endD, substitute, VacationRequestStatus.New, None)).asTry.map {
        case Failure(e) if e.getMessage contains "date conflict occurred" => Failure(new OverlappingDatesException)
        case v => v
      }.execute
    }
  }

  private def getRequestByIdQ(id: Long) = requests.filter(_.id === id)

  /** Достает заявку по его id.
    */
  def getRequestById(id: Long): Option[VacationRequest] = getRequestByIdQ(id).result.headOption.execute


  /** Обновляет статус заявки на "Accepted", если заявка со статусом "New".
    * @throws VacationRequestNotFoundException, если заявки с подобным id нет.
    * @throws InvalidStatusTransitionException, если заявка со статусом "Declined"/"Sent"/"Vacation".
    *
    * @param id id заявки
    */
  def accept(id: Long): Try[VacationRequest] = {
    sql"""
      update vacation_requests
      set status = ${VacationRequestStatus.Accepted}
      where id = $id
      returning id, user_id, vacation_type, start_date, end_date, substitute, status, decline_reason
    """.as[VacationRequest].headOption.asTry.map {
      case Success(None) => Failure(new VacationRequestNotFoundException)
      case Success(Some(v)) => Success(v)
      case Failure(InvalidStatusTransitionException(e)) => Failure(e)
      case Failure(v) => Failure(v)
    }.execute
  }

  /** Обновляет статус заявки на "Declined", если заявка со статусом "New".
    * @throws VacationRequestNotFoundException, если заявки с подобным id нет.
    * @throws InvalidStatusTransitionException, если заявка со статусом "Declined"/"Sent"/"Vacation".
    *
    * @param id id заявки
    */
  def decline(id: Long, declineReason: String): Try[VacationRequest] = {
    sql"""
      update vacation_requests
      set status = ${VacationRequestStatus.Declined}, decline_reason = $declineReason
      where id = $id
      returning id, user_id, vacation_type, start_date, end_date, substitute, status, decline_reason
    """.as[VacationRequest].headOption.asTry.map {
      case Success(None) => Failure(new VacationRequestNotFoundException)
      case Success(Some(v)) => Success(v)
      case Failure(InvalidStatusTransitionException(e)) => Failure(e)
      case Failure(v) => Failure(v)
    }.execute
  }

  /** Удаляет заявку.
    * @param id id заявки
    */
  def delete(id: Long): Option[VacationRequest] = {
    sql"""
      delete from vacation_requests
      where id=$id
      returning id, user_id, vacation_type, start_date, end_date, substitute, status, decline_reason
    """.as[VacationRequest].headOption.execute
  }

  /** Достает заявки по статусу 'status' с заданной страницы.
    * Возвращает пустой лист, если заявок нет.
    *
    * @param status статус заявки
    * @param pageNumber номер страницы
    * @param numberOfRows количество строк в странице
    */
  def getVacationRequestsByStatus(status: VacationRequestStatus.status, pageNumber: Int, numberOfRows: Int): List[VacationRequest] = {
    requests.filter(_.status === status).sortBy(_.startDate).drop((pageNumber-1)*numberOfRows).take(numberOfRows).result.execute.toList
  }

  /** Достает лист актуальных заявок пользователя по его id.
    * @param userId id пользователя
    */
  def getActualVacationRequestsByUserId(userId: Long): List[VacationRequest] = {
    requests.filter(x => x.userId === userId && x.endDate >= LocalDate.now()).result.execute.toList
  }

}


