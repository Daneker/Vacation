package kz.btsd.intranet.vacation.api

import kz.btsd.intranet.vacation.Postgres
import kz.btsd.intranet.vacation.Postgres._
import kz.btsd.intranet.vacation.api.SpecUtils._
import org.http4s._
import org.scalatest.Matchers._
import org.scalatest.{BeforeAndAfterAll, BeforeAndAfterEach, WordSpec}
import slick.jdbc.H2Profile.api._


class UserApiSpec extends WordSpec with BeforeAndAfterEach with BeforeAndAfterAll {
  override def beforeAll(): Unit = {
    Postgres.init()
    sqlu"""delete from users""".execute
  }

  override def afterAll(): Unit = {
    Postgres.close()
  }

  "getUser" when {
    "данные пользователя" should {
      "вернуть Ok" in {
        val response = runReq(path = "/me")
        check(response) should be(true)
      }
    }
    "неправильный path" should {
      "вернуть Not Found" in {
        val response = runReq(path = "/me/notfound")
        check(response, Status.NotFound) should be(true)
      }
    }
    "неправильный header" should {
      "вернуть Unauthorized" in {
        val response = runReq(path = "/me", authHeader = Header("X-User-Emailll", email))
        check(response, Status.Unauthorized) should be(true)
      }
    }
  }
}
