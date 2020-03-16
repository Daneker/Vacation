package kz.btsd.intranet.vacation.api

import java.sql.Date
import java.time.LocalDate

import cats.effect.IO
import io.circe.Json
import io.circe.parser.parse
import io.circe.syntax._
import kz.btsd.intranet.vacation.Postgres
import kz.btsd.intranet.vacation.Postgres._
import kz.btsd.intranet.vacation.api.CommonJsonCodecs._
import kz.btsd.intranet.vacation.api.security._
import kz.btsd.intranet.vacation.dao.{Users, VacationRequests}
import kz.btsd.intranet.vacation.api.SpecUtils._
import kz.btsd.intranet.vacation.model.{VacationRequest, VacationRequestStatus, VacationType}
import org.http4s
import org.http4s._
import org.http4s.circe.jsonOf
import org.scalatest.Matchers._
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, WordSpec}
import slick.jdbc.H2Profile.api._
import slick.jdbc.{PositionedParameters, SetParameter}


import scala.concurrent.ExecutionContext.Implicits.global

class VacationRequestApiSpec extends WordSpec with BeforeAndAfterEach with BeforeAndAfterAll {

  val sd = LocalDate.of(2020, 1, 1)
  val ed = LocalDate.of(2020, 4, 30)

  val vtype = VacationType.Yearly
  var uid: Long = 0
  val substitute = "Someone"
  val hrEmail = "hr1@host"
  val illegalId: Long = 0

  override protected def beforeEach(): Unit = {
    sqlu"""delete from vacation_requests""".execute
  }

  override def beforeAll(): Unit = {
    Postgres.init()

    sqlu"""delete from vacation_requests""".execute
    sqlu"""delete from users""".execute
    val someUser= Users.getUser(email)
    uid = someUser.id
  }

  override def afterAll(): Unit = {
    Postgres.close()
  }


  implicit val jsonDecoder:EntityDecoder[IO, Json] = jsonOf[IO, Json]

  def insertVacationRequest(uid: Long = uid, vtype: VacationType.`type` = vtype, startD: LocalDate = sd,
                            endD: LocalDate = ed, substitute: String = substitute, status: VacationRequestStatus.status = VacationRequestStatus.New):
  VacationRequest = {
    implicit val setType = SetParameter[VacationType.`type`]( (v: VacationType.`type`, pp: PositionedParameters) => { pp.setString(v.toString)})
    implicit val setStatus = SetParameter[VacationRequestStatus.status]( (v: VacationRequestStatus.status, pp: PositionedParameters) => { pp.setString(v.toString)})
    implicit val setDate = SetParameter[LocalDate]((v: LocalDate, pp: PositionedParameters) => pp.setDate(Date.valueOf(v)) )

    sql"insert into vacation_requests (user_id, vacation_type, start_date, end_date, substitute, status) values ($uid, $vtype, $startD, $endD, $substitute, $status) returning id"
      .as[Long].head.map(id => VacationRequest(id, uid, vtype, startD, endD, substitute, status, None)).execute
  }


  def runReqHr(method: Method = Method.GET, path: String, query: http4s.Query = http4s.Query.empty, authHeader: Header =
  Header(`X-User-Email`.toString(), hrEmail),
             mapFn: Request[IO] => Request[IO] = identity): IO[Response[IO]] = {
    runReq(method, path, query, authHeader, mapFn)
  }

  def getExpectedJson(req: VacationRequest): Json = {
    Json.obj(
      ("items", List(Json.obj(
        ("id", req.id.asJson),
        ("type", req.`type`.asJson),
        ("start_date", req.startDate.asJson),
        ("end_date", req.endDate.asJson),
        ("status", req.status.asJson))
      ).asJson )
    )
  }

  "getActualVacationRequests" when {
    "у пользователя есть актуальные запросы" should {
      "вернуть Ok и вернуть список запросов" in {
        val response = runReq(path = "/me/requests")
        val req = VacationRequests.create(uid, vtype, LocalDate.now(), LocalDate.now().plusDays(5), substitute).get
        val expectedJson = getExpectedJson(req)
        check(response, expectedBody = Some(expectedJson)) should be (true)
      }
    }
    "у пользователя нет запросов" should {
      "вернуть Ok и вернуть пустой список" in {
        val response = runReq(path = "/me/requests")
        val json: String = """
          {
            "items": []
          }
        """
        val expectedJson: Json = parse(json).getOrElse(Json.Null)
        check(response, expectedBody = Some(expectedJson)) should be (true)
      }
    }
    "у пользователя есть запросы, но они не актутальные" should {
      "вернуть Ok и вернуть пустой список" in {
        val response = runReq(path = "/me/requests")
        VacationRequests.create(uid, vtype, LocalDate.of(2018, 1, 1), LocalDate.of(2018, 4, 30), substitute).get
        val json: String = """
          {
            "items": []
          }
        """
        val expectedJson: Json = parse(json).getOrElse(Json.Null)
        check(response, expectedBody = Some(expectedJson)) should be (true)
      }
    }
    "у пользователя есть запрос, но актуальна только end date" should {
      "вернуть Ok и вернуть список c этим запросом" in {
        val response = runReq(path = "/me/requests")
        val req = VacationRequests.create(uid, vtype, LocalDate.of(2018, 1, 1), LocalDate.now(), substitute).get
        val expectedJson = getExpectedJson(req)
        check(response, expectedBody = Some(expectedJson)) should be (true)
      }
    }
  }

  "createVacationRequest" when {
    "создать запрос" should {
      "вернуть Ok" in {
        val response = runReq(Method.POST, "/me/requests",
          mapFn = _.withEntity("""{"start_date": "2021-07-11", "end_date": "2021-07-13", "type": "Yearly",  "substitute": "Someone"}"""))
        check(response) should be (true)
      }
    }
    "конфликт дат" should {
      "вернуть BadRequest" in {
        VacationRequests.create(uid, vtype, sd, ed, substitute).get
        val response = runReq(Method.POST, "/me/requests",
          mapFn = _.withEntity(s"""{"start_date": "$sd", "end_date": "$ed", "type": "$vtype",  "substitute": "lalal"}"""))
        check(response, Status.BadRequest) should be (true)
      }
    }
    "дата начала позже даты окончания" should {
      "вернуть BadRequest" in {
        val response = runReq(Method.POST, "/me/requests",
          mapFn = _.withEntity(s"""{"start_date": "$ed", "end_date": "$sd", "type": "$vtype",  "substitute": "llalal"}"""))
        check(response, Status.BadRequest) should be (true)
      }
    }
  }

  "deleteVacationRequest" when {
    "удалить заявку" should {
      "вернуть Ok и удалить заявку" in {
        val req = VacationRequests.create(uid, vtype, sd, ed, substitute).get
        val response = runReq(Method.DELETE, s"/me/requests/${req.id}")
        check(response) should be (true)
      }
    }
    "удалить несуществующую заявку" should {
      "вернуть NotFound" in {
        val response = runReq(Method.DELETE, s"/me/requests/$illegalId")
        check(response, Status.NotFound) should be (true)
      }
    }
    "удалить заявку статуса Vacation" should {
      "вернуть NotFound" in {
        val req = insertVacationRequest(status = VacationRequestStatus.Vacation)
        val response = runReq(Method.DELETE, s"/me/requests/${req.id}")
        check(response, Status.NotFound) should be (true)
      }
    }
    "пользователь не авторизован" should {
      "вернуть Unauthorized" in {
        val req = VacationRequests.create(uid, vtype, sd, ed, substitute).get
        val response = runReq(Method.DELETE, s"/me/requests/${req.id}", authHeader = Header(`X-User-Email`.toString(), "a"))
        check(response, Status.Unauthorized) should be (true)
      }
    }
  }


  "deleteVacation" when {
    "удалить отпуск" should {
      "вернуть Unauthorized и удалить отпуск" in {
        val req = insertVacationRequest(status = VacationRequestStatus.Vacation)
        val response = runReq(Method.DELETE, s"/me/${req.id}")
        check(response, Status.Unauthorized) should be (true)
      }
    }
    "удалить несуществующий отпуск" should {
      "вернуть NotFound" in {
        val response = runReq(Method.DELETE, s"/me/$illegalId")
        check(response, Status.NotFound) should be (true)
      }
    }
    "удалить заявку не статуса Vacation" should {
      "вернуть NotFound" in {
        val req = VacationRequests.create(uid, vtype, sd, ed, substitute).get
        val response = runReq(Method.DELETE, s"/me/${req.id}")
        check(response, Status.NotFound) should be (true)
      }
    }
  }

  // hr services

  "hrGetVacationRequestsByStatus" when {
    "вызван обычным пользователем" should {
      "вернуть NotFound" in {
        val response = runReq(path = "/requests/new", query = http4s.Query("page" -> Some("1")))
        check(response, expectedStatus = Status.NotFound) should be (true)
      }
    }
    "заявки с данным статусом существуют на 1 странице" should {
      "вернуть Ok и лист с 1 VacationRequest" in {
        val response = runReqHr(path = "/requests/new", query = http4s.Query("page" -> Some("1")))
        val req = VacationRequests.create(uid, vtype, sd, ed, substitute).get
        val expectedJson = getExpectedJson(req)
        check(response, expectedBody = Some(expectedJson)) should be (true)
      }
    }
    "заявок с данным статусом нет" should {
      "вернуть Ok и пустой лист" in {
        val response = runReqHr(path = "/requests/new", query = http4s.Query("page" -> Some("1")))
        val json: String = """
          {
            "items": []
          }
        """
        val expectedJson: Json = parse(json).getOrElse(Json.Null)
        check(response, expectedBody = Some(expectedJson)) should be (true)
      }
    }
    "заявки с данным статусом есть, но не на этой странице" should {
      "вернуть Ok и пустой лист" in {
        val response = runReqHr(path = "/requests/new", query = http4s.Query("page" -> Some("2")))
        val json: String = """
          {
            "items": []
          }
        """
        val expectedJson: Json = parse(json).getOrElse(Json.Null)
        check(response, expectedBody = Some(expectedJson)) should be (true)
      }
    }
    "заявки с данным статусом есть, но страница некорректного формата" should {
      "вернуть Ok и первую страницу" in {
        val response = runReqHr(path = "/requests/new", query = http4s.Query("page" -> Some("лр")))
        val req = VacationRequests.create(uid, vtype, sd, ed, substitute).get
        val expectedJson = getExpectedJson(req)
        check(response, expectedBody = Some(expectedJson)) should be (true)
      }
    }
    "некорректный статус" should {
      "вернуть NotFound" in {
        val response = runReqHr(path = "/requests/ne", query = http4s.Query("page" -> Some("1")))
        check(response, Status.NotFound) should be (true)
      }
    }
  }

  "hrAcceptVacationRequests" when {
    "вызван обычным пользователем" should {
      "вернуть NotFound" in {
        val response = runReq(Method.POST, "/requests/accepted")
        check(response, expectedStatus = Status.NotFound) should be (true)
      }
    }
    "принять заявку статуса New" should {
      "вернуть Ok, поменять статус заявок на Accepted" in {
        val req = VacationRequests.create(uid, vtype, sd, ed, substitute).get
        val response = runReqHr(Method.POST, "/requests/accepted", mapFn = _.withEntity(s"""{"ids": ["${req.id}"]}"""))
        check(response) should be (true)
      }
    }
    "нет заявок с такими id" should {
      "вернуть Bad Request и обьект error с парой id -> not_found" in {
        val response = runReqHr(Method.POST, "/requests/accepted", mapFn = _.withEntity(s"""{"ids": ["$illegalId"]}"""))
        val expectedJson = Json.obj(
          ("error", Json.obj((illegalId.toString,
            "not_found".asJson)
          ).asJson)
        ).asJson
        check(response, Status.BadRequest, Some(expectedJson)) should be (true)

      }
    }
    "заявка с таким id существует, но она не статуса New" should {
      "вернуть Bad Request и обьект error с парой id -> invalid_status_transition" in {
        val req = insertVacationRequest(status = VacationRequestStatus.Declined)
        val response = runReqHr(Method.POST, "/requests/accepted", mapFn = _.withEntity(s"""{"ids": ["${req.id}"]}"""))
        val expectedJson = Json.obj(
          ("error", Json.obj((req.id.toString,
            "invalid_status_transition".asJson)
          ).asJson)
        ).asJson
        check(response, Status.BadRequest, Some(expectedJson)) should be(true)
      }
    }
  }


  "hrDeclineVacationRequest" when {
    "вызван обычным пользователем" should {
      "вернуть NotFound" in {
        val response = runReq(Method.POST, "/requests/declined")
        check(response, expectedStatus = Status.NotFound) should be (true)
      }
    }
    "отклонить заявку статуса New" should {
      "вернуть Ok, поменять статус заявки на Declined и добавить причину отказа" in {
        val req = VacationRequests.create(uid, vtype, sd, ed, substitute).get
        val response = runReqHr(Method.POST, "/requests/declined",
          mapFn = _.withEntity(s"""{"id": "${req.id}", "reason" : "just because"}"""))
        check(response) should be (true)
      }
    }
    "нет заявки с таким id" should {
      "вернуть Not Found" in {
        val response = runReqHr(Method.POST, "/requests/declined",
          mapFn = _.withEntity(s"""{"id": "$illegalId", "reason" : "just because"}"""))
        check(response, Status.NotFound) should be (true)

      }
    }
    "заявка с таким id есть, но она не статуса New" should {
      "вернуть BadRequest()" in {
        val req = insertVacationRequest(status = VacationRequestStatus.Accepted)
        val response = runReqHr(Method.POST, "/requests/declined",
          mapFn = _.withEntity(s"""{"id": "${req.id}", "reason" : "just because"}"""))
        check(response, Status.BadRequest) should be(true)
      }
    }
    "заявка с таким id есть, но она уже статуса Declined, и мы хотим поменять причину отказа" should {
      "вернуть Ok" in {
        val req = insertVacationRequest(status = VacationRequestStatus.Declined)
        val response = runReqHr(Method.POST, "/requests/declined",
          mapFn = _.withEntity(s"""{"id": "${req.id}", "reason" : "just becase"}"""))
        check(response) should be(true)
      }
    }
  }

  "hrDeleteVacationRequest" when {
    "вызван обычным пользователем" should {
      "вернуть NotFound" in {
        val req = VacationRequests.create(uid, vtype, sd, ed, substitute).get
        val response = runReq(Method.DELETE, s"/requests/${req.id}")
        check(response, expectedStatus = Status.NotFound) should be (true)
      }
    }
    "удалить заявку" should {
      "вернуть Ok и удалить заявку" in {
        val req = VacationRequests.create(uid, vtype, sd, ed, substitute).get
        val response = runReqHr(Method.DELETE, s"/requests/${req.id}")
        check(response) should be (true)
      }
    }
    "удалить несуществующую заявку" should {
      "вернуть NotFound" in {
        val response = runReqHr(Method.DELETE, s"/requests/$illegalId")
        check(response, Status.NotFound) should be (true)
      }
    }
    "удалить заявку статуса Vacation" should {
      "вернуть NotFound" in {
        val req = insertVacationRequest(status = VacationRequestStatus.Vacation)
        val response = runReqHr(Method.DELETE, s"/requests/${req.id}")
        check(response, Status.NotFound) should be (true)
      }
    }
  }


  "hrDeleteVacation" when {
    "вызван обычным пользователем" should {
      "вернуть NotFound" in {
        val req = insertVacationRequest(status = VacationRequestStatus.Vacation)
        val response = runReq(Method.DELETE, s"/${req.id}")
        check(response, expectedStatus = Status.NotFound) should be (true)
      }
    }
    "удалить отпуск" should {
      "вернуть Unauthorized и удалить отпуск" in {
        val req = insertVacationRequest(status = VacationRequestStatus.Vacation)
        val response = runReqHr(Method.DELETE, s"/${req.id}")
        check(response, Status.Unauthorized) should be (true)
      }
    }
    "удалить несуществующий отпуск" should {
      "вернуть NotFound" in {
        val response = runReqHr(Method.DELETE, s"/$illegalId")
        check(response, Status.NotFound) should be (true)
      }
    }
    "удалить заявку не статуса Vacation" should {
      "вернуть NotFound" in {
        val req = VacationRequests.create(uid, vtype, sd, ed, substitute).get
        val response = runReqHr(Method.DELETE, s"/${req.id}")
        check(response, Status.NotFound) should be (true)
      }
    }
  }

}
