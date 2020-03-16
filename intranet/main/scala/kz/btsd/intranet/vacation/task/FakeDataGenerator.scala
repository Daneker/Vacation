package kz.btsd.intranet.vacation.task
import slick.jdbc.H2Profile.api._
import kz.btsd.intranet.vacation.Postgres._

object FakeDataGenerator {
  def main(args: Array[String]): Unit = {
    val numberOfUsers = 3
    clearDB()
    generateUsers(numberOfUsers)
    generateVacations()
  }


  private def clearDB(): Unit = {
    sqlu"""
      delete from vacation_requests;
      delete from users;
    """.execute
  }

  private def generateUsers(numberOfUsers: Int): Unit = {
    sqlu"""
      insert into users (
          email
      )
      select
          concat(left(md5(random()::text),8),'.',left(md5(random()::text),10),'@btsdigital.kz')
      from generate_series(1, $numberOfUsers)
    """.execute
  }

  private def generateVacations(): Unit = {
    sqlu"""
      with ids as (
        select id from users
      ),
      start_dates as (
        select current_date - s.a AS dates FROM generate_series(2,29,3) AS s(a)
      )
      insert into vacation_requests (
          user_id,
          vacation_type,
          start_date,
          end_date,
          substitute,
          status
      )
      select
          ids.id,
          'Yearly',
          start_dates.dates,
          start_dates.dates + 1,
          'Someone',
          'Vacation'
      from ids, start_dates;
      with ids as (
        select id from users
      ),
      start_dates as (
        select current_date - s.a AS dates FROM generate_series(31,58,3) AS s(a)
      )
      insert into vacation_requests (
          user_id,
          vacation_type,
          start_date,
          end_date,
          substitute,
          status,
          decline_reason
      )
      select
          ids.id,
          'Yearly',
          start_dates.dates,
          start_dates.dates + 1,
          'Someone',
          'Declined',
          'just because'
      from ids, start_dates;
      with ids as (
        select id from users
      ),
      start_dates as (
        select current_date + s.a AS dates FROM generate_series(1,3,3) AS s(a)
      )
      insert into vacation_requests (
          user_id,
          vacation_type,
          start_date,
          end_date,
          substitute,
          status
      )
      select
          ids.id,
          'Yearly',
          start_dates.dates,
          start_dates.dates + 1,
          'Someone',
          'Sent'
      from ids, start_dates;

      with ids as (
        select id from users
      ),
      start_dates as (
        select current_date + s.a AS dates FROM generate_series(6,9,3) AS s(a)
      )
      insert into vacation_requests (
          user_id,
          vacation_type,
          start_date,
          end_date,
          substitute,
          status
      )
      select
          ids.id,
          'Yearly',
          start_dates.dates,
          start_dates.dates + 1,
          'Someone',
          'Accepted'
      from ids, start_dates;

      with ids as (
        select id from users
      ),
      start_dates as (
        select current_date + s.a AS dates FROM generate_series(12,15,3) AS s(a)
      )
      insert into vacation_requests (
          user_id,
          vacation_type,
          start_date,
          end_date,
          substitute,
          status
      )
      select
          ids.id,
          'Yearly',
          start_dates.dates,
          start_dates.dates + 1,
          'Someone',
          'New'
      from ids, start_dates;
    """.execute
  }




}
