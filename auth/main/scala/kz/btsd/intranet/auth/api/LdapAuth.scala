package kz.btsd.intranet.auth.api

import cats.effect.IO
import io.circe.generic.auto._
import kz.btsd.intranet.auth.api.security._
import kz.btsd.intranet.auth.service.{InvalidLdapCredentialsException, LdapService}
import org.http4s.circe.{jsonOf, _}
import org.http4s.dsl.Http4sDsl
import org.http4s.{EntityDecoder, HttpRoutes, Request}
import cats.implicits._

/**
  * аутентификация через ldap
  */
object LdapAuth extends Http4sDsl[IO] {

  val api = HttpRoutes.of[IO] {
    // логин и получение jwt токена
    case  req @ POST -> Root / "login" / "ldap" => doLogin(req)
  }

  /**
    * Аутентифицирует юзера и возвращает токен
    */
  private def doLogin(req: Request[IO]) = {
    req.as[LoginData].flatMap { data =>
      val resp =
        for {
          ldapUser <- IO.fromTry(LdapService.authenticate(data.login, data.pw))
          token <- JwtAuth.create(UserIdentity(ldapUser.email))
          res <- Ok()
        } yield JwtAuth.embed(res, token)

      resp.recoverWith {
        case _:InvalidLdapCredentialsException => unauthorizedRespIO
      }
    }
  }

  implicit val loginDataDecoder:EntityDecoder[IO, LoginData] = jsonOf[IO, LoginData]
  protected case class LoginData(login: String, pw: String)
}
