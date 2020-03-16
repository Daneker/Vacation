package kz.btsd.intranet.auth

import cats.effect._
import cats.syntax.semigroupk._
import io.circe.Json
import io.circe.syntax._
import kz.btsd.intranet.auth.api.security._
import org.apache.logging.log4j.LogManager
import org.http4s.circe._
import org.http4s.dsl.io._
import org.http4s.{Header, Request, Response}
import tsec.authentication.{TSecAuthService, _}

package object api {
  private[api] val unauthorizedRespIO = IO.pure(Response[IO](Unauthorized))

  private val securedRoutes: JwtAuthService = TSecAuthService {
    // Вызывается из проксирующего сервиса (например, nginx) для определения того,
    // можно ли дальше отправлять запрос в upstream. В случае успеха возвращает заголовки с данными о юзере
    case GET -> Root / "internal" / "auth_info" asAuthed user => sendAuthInfo(user)

    case GET -> Root / "logout" asAuthed _ => Ok().map(_.removeCookie(AUTH_COOKIE_NAME))
  }

  /**
    * отправляет в заголовках данные о текущем юзере
    */
  private def sendAuthInfo(ident: UserIdentity) = {
    // в ответ добавим заголовки для передачи в upstream сервисы
    Ok(Header("X-User-Email", ident.email))
  }

  // все доступное АПИ
  val routes = GoogleSignIn.api <+> LdapAuth.api <+>  JwtAuthRequestHandler.liftService(securedRoutes)
}
