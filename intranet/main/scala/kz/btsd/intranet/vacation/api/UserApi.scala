package kz.btsd.intranet.vacation.api

import cats.effect.IO
import io.circe._
import io.circe.syntax._
import kz.btsd.intranet.vacation.api.security._
import kz.btsd.intranet.vacation.dao.Employees
import kz.btsd.intranet.vacation.model.User
import org.http4s.Response
import org.http4s.circe._
import org.http4s.dsl.Http4sDsl
import tsec.authentication._
import tsec.authorization
import security._

object UserApi extends Http4sDsl[IO] {

  val services: AuthService = AuthService {
    case GET -> Root / "me" asAuthed user => getUserInfo(user)
  }

  /** Возвращает информацию о пользователе.
    * @param actor объект User(пользователь)
    */
  private def getUserInfo(actor: User)(implicit authzInfo: authorization.AuthorizationInfo[IO, Role, User]): IO[Response[IO]] = {
    for {
      role <- authzInfo.fetchInfo(actor)
      employee = actor.employee
      json = JsonObject(
        ("name", employee.name.asJson),
        ("email", employee.email.asJson),
        ("department", employee.department.asJson),
        ("position", employee.position.asJson),
        ("role", role.roleRepr.asJson)
      ).asJson
      resp <- Ok(json)
    } yield resp
  }
}