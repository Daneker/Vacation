package kz.btsd.intranet.vacation.dao

import kz.btsd.intranet.vacation.Postgres._
import kz.btsd.intranet.vacation.model.User
import slick.jdbc.PostgresProfile.api._
import slick.lifted.Tag

import scala.concurrent.ExecutionContext.Implicits.global

private class Users(tag: Tag) extends Table[User](tag, "users") {
  def id = column[Long]("id", O.PrimaryKey)
  def email = column[String]("email")

  def * = (id, email) <> (User.tupled, User.unapply)
}

/**
  * DAO для работы с пользователем
  */
object Users {
  private val users = TableQuery[Users]

  /** Возвращает пользователя, если его нет в базе создает нового.
    * @param email имейл пользователя
    */
  def getUser(email: String): User = {
    val getUserByEmailA = users.filter(_.email === email).result
    val getOrCreateUserA = getUserByEmailA.headOption.flatMap {
        case Some(value) => DBIO.successful(value)
        case _ => DBIO.seq((users.map(_.email) += email).asTry).andThen(getUserByEmailA.head)
      }
      .map(user => User(id = user.id, email = user.email))
    getOrCreateUserA.execute
  }

  /** Возвращает пользователя по id.
    * @param id id пользователя
    */
  def getUserById(id: Long): Option[User] = users.filter(_.id === id).result.headOption.execute

}
