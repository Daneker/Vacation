package kz.btsd.intranet.auth

import cats.effect.{ConcurrentEffect, ContextShift, ExitCode, IO, Timer}
import org.http4s.server.blaze.BlazeServerBuilder
import fs2.Stream
import scala.concurrent.ExecutionContext.Implicits.global
import org.http4s.implicits._
import org.http4s.server.Router

object Server {

  implicit val cs:ContextShift[IO] = IO.contextShift(global)
  implicit val timer: Timer[IO] = IO.timer(global)

  private val port = cfg.getInt("port")

  private val httpApp = Router(
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


