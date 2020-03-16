package kz.btsd.intranet.vacation.dao

import java.sql.Date
import java.time.LocalDate

import kz.btsd.intranet.vacation.Postgres._
import kz.btsd.intranet.vacation.{Postgres, cfg}
import kz.btsd.intranet.vacation.model.{VacationRequest, VacationRequestStatus, VacationType}
import org.scalatest.Matchers._
import org.scalatest.TryValues._
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, WordSpec}
import slick.jdbc.PostgresProfile.api._
import slick.jdbc.{PositionedParameters, SetParameter}

import scala.concurrent.ExecutionContext.Implicits.global

class VacationRequestSpec extends WordSpec with BeforeAndAfterEach with BeforeAndAfterAll {


  val sd = LocalDate.of(2019, 1, 1)
  val ed = LocalDate.of(2019, 4, 30)

  val vtype = VacationType.Yearly
  var uid: Long = 0
  val substitute = "Someone"

  override protected def beforeEach(): Unit = {
    sqlu"""delete from vacation_requests""".execute
  }

  override def beforeAll(): Unit = {
    Postgres.init()

    sqlu"""delete from vacation_requests""".execute
    sqlu"""delete from users""".execute
    val someUser= Users.getUser("daneker.nurgaliyeva@btsdigital.kz")
    uid = someUser.id
  }

  override def afterAll(): Unit = {
    Postgres.close()
  }

  private val itemsPerPage = cfg.getInt("pagination.items_per_page")

//  используйте данный trait, чтобы добавить до теста запись с дефолтными параметрами
  trait initialCase {
      VacationRequests.create(uid, vtype, sd, ed, substitute)
  }

  def insertVacationRequest(uid: Long = uid, vtype: VacationType.`type` = vtype, startD: LocalDate = sd,
  endD: LocalDate = ed, substitute: String = substitute, status: VacationRequestStatus.status = VacationRequestStatus.New):
  VacationRequest = {
    implicit val setType = SetParameter[VacationType.`type`]( (v: VacationType.`type`, pp: PositionedParameters) => { pp.setString(v.toString)})
    implicit val setStatus = SetParameter[VacationRequestStatus.status]( (v: VacationRequestStatus.status, pp: PositionedParameters) => { pp.setString(v.toString)})
    implicit val setDate = SetParameter[LocalDate]((v: LocalDate, pp: PositionedParameters) => pp.setDate(Date.valueOf(v)) )

    sql"insert into vacation_requests (user_id, vacation_type, start_date, end_date, substitute, status) values ($uid, $vtype, $startD, $endD, $substitute, $status) returning id"
      .as[Long].head.map(id => VacationRequest(id, uid, vtype, startD, endD, substitute, status, None)).execute
  }

  "createRequest" when {
    "если дата окончания < дата начала" should {
      "вернуть ошибку" in {
        val (startDate, endDate) = (LocalDate.of(2019, 1, 20), LocalDate.of(2019, 1, 12))
        val newReq = VacationRequests.create(uid, vtype, startDate, endDate, substitute)
        newReq.failure.exception shouldBe a[IllegalArgumentException]
      }
    }
    "если в базе нет заявки в этом промежутке" should {
      "создать и вернуть заявку" in {
        val (startDate, endDate) = (LocalDate.of(2019, 1, 20), LocalDate.of(2019, 1, 25))
        val newReq = VacationRequests.create(uid, vtype, startDate, endDate, substitute)
        newReq should not be (null)
        newReq.isSuccess should be(true)
      }
    }
    "если в базе уже есть такой промежуток" should {
      "вернуть ошибку, если в базе уже есть заявка с подобным startDate (sd = startDate)" in new initialCase {
        val newReq = VacationRequests.create(uid, vtype, sd, LocalDate.of(2019, 8, 30), substitute)
        newReq.failure.exception shouldBe a[OverlappingDatesException]
      }
      "если в базе уже есть заявка с подобным endDate (ed = endDate)" in new initialCase {
        val newReq = VacationRequests.create(uid, vtype, LocalDate.of(2019, 1, 15), ed, substitute)
        newReq.failure.exception shouldBe a[OverlappingDatesException]
      }
      "если в базе уже есть заявка в подобном промежутке (sd << startDate << ed)" in new initialCase {
        val newReq = VacationRequests.create(uid, vtype, LocalDate.of(2019, 2, 26), LocalDate.of(2019, 6, 30), substitute)
        newReq.failure.exception shouldBe a[OverlappingDatesException]
      }
      "если в базе уже есть заявка в подобном промежутке (sd << endDate << ed)" in new initialCase {
        val newReq = VacationRequests.create(uid, vtype, LocalDate.of(2019, 1, 20), LocalDate.of(2019, 3, 30), substitute)
        newReq.failure.exception shouldBe a[OverlappingDatesException]
      }
      "если в базе уже есть заявка в подобном промежутке (startDate << sd, ed << endDate)" in new initialCase {
        val newReq = VacationRequests.create(uid, vtype, LocalDate.of(2018, 12, 24), LocalDate.of(2019, 6, 30), substitute)
        newReq.failure.exception shouldBe a[OverlappingDatesException]
      }
    }
  }

  "decline request" when {
    "decline - если заявка новая" should {
      "ничего не вернуть" in {
        val req = VacationRequests.create(uid, vtype, sd, ed, substitute).get
        val declinedReq = VacationRequests.decline(req.id, "lol")
        declinedReq.isSuccess should be(true)
      }
    }
    "decline - если заявки с подобной id не существует" should {
      "вернуть ошибку VacationRequestNotFoundException" in {
        val invalidId = 10000001
        val declinedReq = VacationRequests.decline(invalidId, "lol")
        declinedReq.failure.exception shouldBe a[VacationRequestNotFoundException]
      }
    }
    "decline - если заявка уже отменена" should {
      "ничего не вернуть" in {
        val req = insertVacationRequest(status = VacationRequestStatus.Declined)
        val declinedReq = VacationRequests.decline(req.id, "lol")
        declinedReq.isSuccess should be(true)
      }
    }
    "decline - если заявка уже принята" should {
      "вернуть ошибку" in {
        val req = insertVacationRequest(status = VacationRequestStatus.Accepted)
        val declinedReq = VacationRequests.decline(req.id, "lol")
        declinedReq.failure.exception shouldBe a[InvalidStatusTransitionException]
      }
    }
    "decline - если заявка уже отправлена в 1с" should {
      "вернуть ошибку" in {
        val req = insertVacationRequest(status = VacationRequestStatus.Sent)
        val declinedReq = VacationRequests.decline(req.id, "lol")
        declinedReq.failure.exception shouldBe a[InvalidStatusTransitionException]
      }
    }
    "decline - если завка уже обращена в отпуск" should {
      "вернуть ошибку" in {
        val req = insertVacationRequest(status = VacationRequestStatus.Vacation)
        val declinedReq = VacationRequests.decline(req.id, "lol")
        declinedReq.failure.exception shouldBe a[InvalidStatusTransitionException]
      }
    }
  }

  "accept request" when {
    "accept - если заявка новая" should {
      "ничего не вернуть" in {
        val req = VacationRequests.create(uid, vtype, sd, ed, substitute).get
        val declinedReq = VacationRequests.accept(req.id)
        declinedReq.isSuccess should be(true)
      }
    }
    "accept - если заявки с подобной id не существует" should {
      "вернуть ошибку VacationRequestNotFoundException" in {
        val invalidId = 10000001
        val declinedReq = VacationRequests.accept(invalidId)
        declinedReq.failure.exception shouldBe a[VacationRequestNotFoundException]
      }
    }
    "accept - если заявка уже отменена" should {
      "вернуть ошибку" in {
        val req = insertVacationRequest(status = VacationRequestStatus.Declined)
        val declinedReq = VacationRequests.accept(req.id)
        declinedReq.failure.exception shouldBe a[InvalidStatusTransitionException]
      }
    }
    "accept - если заявка уже принята" should {
      "ничего не вернуть" in {
        val req = insertVacationRequest(status = VacationRequestStatus.Accepted)
        val declinedReq = VacationRequests.accept(req.id)
        declinedReq.isSuccess should be(true)
      }
    }
    "accept - если заявка уже отправлена в 1с" should {
      "вернуть ошибку" in {
        val req = insertVacationRequest(status = VacationRequestStatus.Sent)
        val declinedReq = VacationRequests.accept(req.id)
        declinedReq.failure.exception shouldBe a[InvalidStatusTransitionException]
      }
    }
    "accept - если завка уже обращена в отпуск" should {
      "вернуть ошибку" in {
        val req = insertVacationRequest(status = VacationRequestStatus.Vacation)
        val declinedReq = VacationRequests.accept(req.id)
        declinedReq.failure.exception shouldBe a[InvalidStatusTransitionException]
      }
    }
  }

  "delete request" when {
    "delete - если заявка  есть" should {
      "вернуть удаленную заявку" in {
        val req = VacationRequests.create(uid, vtype, sd, ed, substitute).get
        val deletedReq = VacationRequests.delete(req.id)
        deletedReq should not be (None)
      }
    }
    "delete - если заявки нет" should {
      "вернуть none" in {
        val invalidId = 10000001
        val deletedReq = VacationRequests.delete(invalidId)
        deletedReq should be (None)
      }
    }
  }


  "get requests by status" when {
    "если заявок c этим статусом не было" should {
      "вернуть пустой лист" in {
        for {status <- VacationRequestStatus.values} {
          VacationRequests.getVacationRequestsByStatus(status, 1, itemsPerPage) should be(empty)
        }
      }
    }
    "если заявки с этим статусом существуют" should {
      "вернуть лист заявок" in {
        for {status <- VacationRequestStatus.values} {
          try{
            insertVacationRequest(status = status)
            val res = VacationRequests.getVacationRequestsByStatus(status, 1, itemsPerPage)
            res.forall(_.status == status) should be (true)
            res should not be empty
          }
          finally {
            sqlu"""delete from vacation_requests""".execute
          }
        }
      }
    }
  }


}
