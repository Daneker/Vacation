package kz.btsd.intranet.vacation.api

import java.time.LocalDate

import cats.effect.IO
import cats.implicits._
import io.circe.generic.extras.ConfiguredJsonCodec
import io.circe.syntax._
import io.circe.{Encoder, JsonObject}
import kz.btsd.intranet.vacation._
import kz.btsd.intranet.vacation.api.CommonJsonCodecs._
import kz.btsd.intranet.vacation.api.security._
import kz.btsd.intranet.vacation.dao.{InvalidStatusTransitionException, OverlappingDatesException, VacationRequestNotFoundException, VacationRequests}
import kz.btsd.intranet.vacation.model.{User, VacationRequest, VacationRequestStatus, VacationType}
import kz.btsd.intranet.vacation.service.VacationRequestService
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import org.http4s.{EntityDecoder, EntityEncoder, Request, Response}
import tsec.authentication._

import scala.util.Try

object VacationRequestApi extends Http4sDsl[IO] {

  private val itemsPerPage = cfg.getInt("pagination.items_per_page")

  private implicit val encodeException: Encoder[Throwable] = {
    case _: VacationRequestNotFoundException => "not_found".asJson
    case _: InvalidStatusTransitionException => "invalid_status_transition".asJson
    case _: OverlappingDatesException => "overlapping_dates".asJson
    case _ => "unknown_error".asJson
  }

  private implicit val acceptMapEntityEncoder: EntityEncoder[IO, Map[Long, Throwable]] = jsonEncoderOf[IO, Map[Long, Throwable]]

  private object ErrorsHandler extends HttpErrorHandler({
    case e @ (_:InvalidStatusTransitionException | _: OverlappingDatesException) => errorResponse(encodeException(e))
    case _: VacationRequestNotFoundException => NotFound()
  })

  private implicit val newVacationRequestDataDecoder: EntityDecoder[IO, NewVacationRequestData] = jsonOf[IO, NewVacationRequestData]

  val services: AuthService = ErrorsHandler(AuthService {
    case GET        -> Root / "me" / "requests" asAuthed user => getActualVacationRequests(user)
    case req @ POST -> Root / "me" / "requests" asAuthed user => createVacationRequest(user, req.request)
    case DELETE     -> Root / "me" / "requests" / VacationRequestParam(vr) asAuthed user if vr.userId == user.id => deleteVacationRequest(vr.id)
    case DELETE     -> Root / "me" / VacationParam(vr) asAuthed user if vr.userId == user.id => deleteVacationRequest(vr.id)
  })


  /** Возвращает JSON со списком актуальных заявок пользователя, если они есть.
    * Возвращает JSON c пустым списком, если нет актуальных заявок.
    *
    * @param user пользователь c полями id и email
    */
  private def getActualVacationRequests(user: User): IO[Response[IO]] = {
    for {
      reqs <- IO(VacationRequestService.getActualVacationRequestsByUserId(user.id))
      jsonRes = JsonObject(
        ("items", reqs.asJson)
      ).asJson
      res <- Ok(jsonRes)
    } yield res
  }

  /** Создает заявку под текущим пользователем.
    * Возвращает созданную заявку.
    *
    * @throws BadRequest, если не удается создать заявку.
    * @param actor пользователь c полями id и email
    * @param req POST запрос в форме JSON c полями startDate, endDate, type, substitute
    */
  private def createVacationRequest(actor: User, req: Request[IO]): IO[Response[IO]] = {
    req.as[NewVacationRequestData].flatMap { vr =>
      for {
        req <- IO.fromTry(VacationRequestService.create(actor, vr.`type`, vr.startDate, vr.endDate, vr.substitute))
        res <- Ok(req)
      } yield res
    }
  }

  /** Удаляет заявку текущего пользователя и возвращает удаленную заявку.
    *
    * @throws NotFound, если указанной заявки нет.
    * @param id id заявки, которую хотим удалить
    */
  private def deleteVacationRequest(id: Long): IO[Response[IO]] = {
    for {
      _ <- IO(VacationRequestService.delete(id))
      res <- Ok()
    } yield res
  }


  /** Экстрактор для запросов, которые еще не стали реальными отпусками */
  object VacationRequestParam {
    def unapply(idStr: String): Option[VacationRequest] = {
      Try(idStr.toLong).toOption.flatMap(VacationRequests.getRequestById).filter(_.status != VacationRequestStatus.Vacation)
    }
  }

  /** Экстрактор для запросов, которые стали отпусками */
  object VacationParam {
    def unapply(idStr: String): Option[VacationRequest] = {
      Try(idStr.toLong).toOption.flatMap(VacationRequests.getRequestById).filter(_.status == VacationRequestStatus.Vacation)
    }
  }

  object VacationRequestStatusParam {
    def unapply(statusStr: String): Option[VacationRequestStatus.status] = {
      statusStr match {
        case "accepted" => Some(VacationRequestStatus.Accepted)
        case "declined" => Some(VacationRequestStatus.Declined)
        case "sent" => Some(VacationRequestStatus.Sent)
        case "new" => Some(VacationRequestStatus.New)
        case _ => None
    }
  }}

  implicit val acceptRequestsDataDecoder:EntityDecoder[IO, AcceptRequestsData] = jsonOf[IO, AcceptRequestsData]
  implicit val declineRequestDataDecoder:EntityDecoder[IO, DeclineRequestData] = jsonOf[IO, DeclineRequestData]

  object PageQueryParamMatcher extends FallbackQueryParamDecoderMatcher[Int]("page", 1)

  val hrServices: AuthService = ErrorsHandler(AuthService.withAuthorization(HrOnly) {
    case GET        -> Root / "requests" / VacationRequestStatusParam(status) :? PageQueryParamMatcher(page) asAuthed _ =>
      getVacationRequestsByStatus(status, page)
    case req @ POST -> Root / "requests" / "accepted" asAuthed _ => acceptVacationRequests(req.request)
    case req @ POST -> Root / "requests" / "declined" asAuthed _ => declineVacationRequest(req.request)
    case DELETE     -> Root / "requests" / VacationRequestParam(vr) asAuthed _ => deleteVacationRequest(vr.id)
    case DELETE     -> Root / VacationParam(vr) asAuthed _ => deleteVacationRequest(vr.id)
  })

  /** Возвращает JSON со списком заявок по статусу на указанной странице, если они есть.
    * Возвращает JSON c пустым списком, если нет заявок с указанным статусом на указанной странице.
    *
    * @param status статус заявки
    * @param pageNumber номер страницы
    */
  private def getVacationRequestsByStatus(status: VacationRequestStatus.status, pageNumber: Int): IO[Response[IO]] = {
      for {
        reqs <- IO(VacationRequestService.getVacationRequestsByStatus(status, pageNumber, itemsPerPage))
        jsonRes = JsonObject(
          ("items", reqs.asJson)
        ).asJson
        res <- Ok(jsonRes)
      } yield res
  }

  /** Обновляет статус заявок на "Accepted".
    * Возвращает id заявок, чей статус не удалось обновить на "Accepted".
    *
    * @param req JSON со списком id заявок
    */
  private def acceptVacationRequests(req: Request[IO]): IO[Response[IO]] = {
    req.as[AcceptRequestsData].flatMap { data =>
      for {
         m <- IO(VacationRequestService.accept(data.ids))
         res <- if (m.isEmpty) Ok() else errorResponse(m.asJson)
      } yield res
    }
  }

  /** Обновляет статус заявки на "Declined" и причину отказа заявки.
    * @throws BadRequest, если указанной заявки нет или если указанная заявка есть, но не статуса New.
    *
    * @param req JSON со списком id заявок
    */
  private def declineVacationRequest(req: Request[IO]): IO[Response[IO]] = {
    req.as[DeclineRequestData].flatMap { data =>
      for {
        _ <- IO.fromTry(VacationRequestService.decline(data.id, data.reason))
        res <- Ok()
      } yield res
    }
  }

  @ConfiguredJsonCodec protected case class AcceptRequestsData(ids: Set[Long])

  @ConfiguredJsonCodec protected case class DeclineRequestData(id: Long, reason: String)

  @ConfiguredJsonCodec protected case class NewVacationRequestData(startDate: LocalDate, endDate: LocalDate, `type`: VacationType.`type`, substitute: String)
}