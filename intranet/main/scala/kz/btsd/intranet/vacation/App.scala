package kz.btsd.intranet.vacation

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits._
import kz.btsd.intranet.vacation.dao.Employees
import kz.btsd.intranet.vacation.service.EmailService.send

object App extends IOApp {

  override def run(args: List[String]) = {
    (
      init() *> Server.stream.compile.drain.as(ExitCode.Success)
    )
    .guarantee(cleanUp())
  }

  private def init() = IO {
    Postgres.init()
    Employees
    val set = Set("daniker.ktl@gmail.com")
    send("hello", "hello", set)
  }

  private def cleanUp() = IO {
    Postgres.close()
  }
}
