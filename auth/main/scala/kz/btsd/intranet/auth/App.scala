package kz.btsd.intranet.auth

import cats.effect.{ExitCode, IO, IOApp}
import org.http4s.HttpRoutes
import org.http4s.dsl.io._
import org.http4s.implicits._
import org.http4s.server.blaze.BlazeServerBuilder
import cats.implicits._

object App extends IOApp {

  override def run(args: List[String]) = {
    Server.stream.compile.drain.as(ExitCode.Success)
  }
}