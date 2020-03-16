package kz.btsd.intranet.vacation.dao


import kz.btsd.intranet.vacation.Postgres
import kz.btsd.intranet.vacation.model.User
import org.scalatest.{BeforeAndAfterAll, WordSpec}
import org.scalatest.Matchers._
import slick.jdbc.PostgresProfile.api._
import kz.btsd.intranet.vacation.Postgres._

import scala.concurrent.ExecutionContext.Implicits.global

class UsersSpec extends WordSpec with BeforeAndAfterAll {

  override def beforeAll(): Unit = {
    Postgres.init()

    sqlu"""delete from vacation_requests""".execute
    sqlu"""delete from users""".execute
  }

  override def afterAll(): Unit = {
    Postgres.close()
  }

  def createUser(email: String): User = {
    sql"insert into users (email) values ($email) returning id"
      .as[Long].head.map(id => User(id, email)).execute
  }


  "getUser" when {
    "если в базе нет такого пользователя" should {
      "создать и вернуть пользователя" in {
        Users.getUser("daneker.nurgaliyeva@nu.edu.kz") should not be (null)
      }
    }
  }

}
