package kz.btsd.intranet.vacation

import cats.effect.{ContextShift, ExitCode, IO, Timer}
import fs2.Stream
import org.http4s.server.Router
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.implicits._
import scala.concurrent.ExecutionContext.Implicits.global

object Server {

  implicit val cs:ContextShift[IO] = IO.contextShift(global)
  implicit val timer: Timer[IO] = IO.timer(global)

  private val port = cfg.getInt("port")

  val httpApp = Router(
    "/" -> api.routes
  ).orNotFound

  /**
    *
    */
  def stream: Stream[IO, ExitCode] =
    for {
      exitCode <- BlazeServerBuilder[IO]
        .bindHttp(port, "0.0.0.0")
        .withHttpApp(httpApp)
        .withoutBanner
        .serve
    } yield exitCode
}


