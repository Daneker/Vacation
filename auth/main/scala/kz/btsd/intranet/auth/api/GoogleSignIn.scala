package kz.btsd.intranet.auth.api

import java.util.Collections

import kz.btsd.intranet.auth.cfg
import cats.effect.IO
import io.circe.generic.auto._
import kz.btsd.intranet.auth.api.security._
import org.http4s.circe.{jsonOf, _}
import org.http4s.{EntityDecoder, HttpRoutes, Request, Response}
import org.http4s.dsl.Http4sDsl
import com.google.api.client.googleapis.auth.oauth2._
import com.google.api.client.http.javanet.NetHttpTransport
import com.google.api.client.json.jackson2.JacksonFactory
import org.apache.logging.log4j.LogManager
//import org.http4s.server.middleware._

import scala.util.Try
import cats.implicits._

object GoogleSignIn extends Http4sDsl[IO] {
  private val log = LogManager.getLogger

  val api = HttpRoutes.of[IO] {
    case req @ POST -> Root / "login" / "google" => doGoogleSignIn(req)
  }


  private val verifier = new GoogleIdTokenVerifier.Builder(new NetHttpTransport(),
    JacksonFactory.getDefaultInstance)
    .setAudience(Collections.singletonList(cfg.getString("google_signin.client_id")))
    .build()

  private def doGoogleSignIn(req: Request[IO]): IO[Response[IO]] = {
    req.as[GoogleSignInData].flatMap { data =>
      val resp =
        for {
          googleUser <- IO.fromTry(verifyToken(data.token))
          token <- JwtAuth.create(UserIdentity(googleUser.email))
          res <- Ok()
        } yield JwtAuth.embed(res, token)

      resp.recoverWith {
        case e: InvalidIdTokenException => {
          log.error("", e)
          unauthorizedRespIO
        }
        case _: IllegalArgumentException => BadRequest() // может возникать при передаче вместо валидного токена всякой ерунды
      }
    }
  }

  private def verifyToken(token: String): Try[GoogleUser] = Try {
    val resOpt = for {
      token <- Option(verifier.verify(token)) if token.getPayload.getHostedDomain == "btsdigital.kz"
      payload = token.getPayload
    } yield GoogleUser(email = payload.getEmail, name = payload.get("name").asInstanceOf[String])

    if(resOpt.isEmpty) {
      throw new InvalidIdTokenException
    }

    resOpt.get
  }



  protected case class GoogleSignInData(token: String)
  implicit val tokenDataDecoder:EntityDecoder[IO, GoogleSignInData] = jsonOf[IO, GoogleSignInData]

  protected case class GoogleUser(email: String, name: String)

  protected class InvalidIdTokenException extends RuntimeException


}
