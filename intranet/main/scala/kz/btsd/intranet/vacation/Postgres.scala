package kz.btsd.intranet.vacation

import java.time.LocalDate

import slick.jdbc.{GetResult, PositionedResult}
import slick.jdbc.PostgresProfile.api._

import scala.concurrent.Await
import scala.concurrent.duration.Duration

object Postgres {
  private var psql: Database = _

  def init(): Unit = {
    assert(psql == null, "psql is not null")
    psql = Database.forConfig("psql", cfg)
  }

  def close(): Unit = {
    psql.close()
    psql = null
  }


  /**
    * доп. методы для запросов
    */
  implicit class DBIOActionHelper[R](val action: DBIOAction[R, NoStream, Nothing]) extends AnyVal {

    /**
      * выполняет запрос и возвращает результат, либо бросает исключение
      */
    def execute = Await.result(psql.run(action), Duration.Inf)
  }

  implicit val getLocalDateResult = GetResult(rs => rs.nextDate().toLocalDate)


}
