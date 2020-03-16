package kz.btsd.intranet.auth.api

import cats.Id
import cats.effect.IO
import io.circe.{Encoder, Json, ObjectEncoder}
import io.circe.export.Exported
import kz.btsd.intranet.auth._
import org.http4s.EntityDecoder
import tsec.authentication.{AugmentedJWT, JWTAuthenticator, SecuredRequestHandler, TSecAuthService, TSecCookieSettings, UserAwareService}
import tsec.mac.jca.{HMACSHA256, MacSigningKey}
import org.http4s.circe.{jsonOf, _}

import scala.concurrent.duration._

package object security {

  case class UserIdentity(email: String)

  type JwtToken = AugmentedJWT[HMACSHA256, UserIdentity]
  type JwtAuthService = TSecAuthService[UserIdentity , JwtToken, IO]
  type JwtAwareService = UserAwareService[UserIdentity , JwtToken, IO]

  private val authenticatorCfg = cfg.getConfig("authenticator")
  private val cookieCfg = authenticatorCfg.getConfig("cookie")

  val AUTH_COOKIE_NAME = cookieCfg.getString("name")
  private val maxIdle = if(authenticatorCfg.hasPath("max_idle")) Some(authenticatorCfg.getInt("max_idle").minutes) else None

  val JwtAuth = {
    import io.circe.generic.auto._

    val expiryDuration = authenticatorCfg.getInt("expiry").minutes

    val signingKey: MacSigningKey[HMACSHA256] = HMACSHA256.generateKey[Id]

    JWTAuthenticator.pstateless.inCookie[IO, UserIdentity, HMACSHA256](
      TSecCookieSettings(
        cookieName = AUTH_COOKIE_NAME,
        secure = cookieCfg.getBoolean("secure"),
        expiryDuration = expiryDuration,
        maxIdle = maxIdle
      ),
      signingKey
    )
  }

  val JwtAuthRequestHandler = SecuredRequestHandler(JwtAuth)
}
