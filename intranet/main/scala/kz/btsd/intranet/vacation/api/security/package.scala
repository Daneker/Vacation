package kz.btsd.intranet.vacation.api

import cats.data.OptionT
import cats.effect.IO
import kz.btsd.intranet.vacation.dao.Users
import kz.btsd.intranet.vacation.model.User
import org.http4s.{Request, Response}
import org.http4s.util.CaseInsensitiveString
import tsec.authentication.{Authenticator, SecuredRequest, SecuredRequestHandler, TSecAuthService}
import cats.implicits._
import tsec.authorization.{AuthGroup, AuthorizationInfo, BasicRBAC, SimpleAuthEnum}
import kz.btsd.intranet.vacation.cfg
import scala.concurrent.duration.FiniteDuration

package object security {
  val `X-User-Email` = CaseInsensitiveString("X-User-Email")

  type PlainEmail = String
  type AuthService = TSecAuthService[User , PlainEmail, IO]

  /**
    * Аутентификатор по заголовку с емайл. Доверяем ему сразу, т.к. запрос сюда попадет только после проверки в сервисе auth
    */
  private val PlainEmailAuth:Authenticator[IO, PlainEmail, User, PlainEmail] = new Authenticator[IO, PlainEmail, User, PlainEmail] {
    def extractRawOption(request: Request[IO]): Option[String] = request.headers.get(`X-User-Email`).map(_.value)

    def parseRaw(raw: String, request: Request[IO]): OptionT[IO, SecuredRequest[IO, User, PlainEmail]] = {
      OptionT(IO {
        val u = Users.getUser(raw)
        if (u.employeeOpt.nonEmpty) // есть некоторая вероятность, что юзера не будет в csv. Таких просто не пускаем
          SecuredRequest(request, u, raw).some
        else
          None
      })
    }

    // методы не нужны
    def expiry = ???
    def maxIdle: Option[FiniteDuration] = None
    def create(body: PlainEmail) = ???
    def update(authenticator: PlainEmail) = ???
    def discard(authenticator: PlainEmail) = ???
    def renew(authenticator: PlainEmail) = ???
    def refresh(authenticator: PlainEmail) = ???
    def embed(response: Response[IO], authenticator: PlainEmail) = ???
    def afterBlock(response: Response[IO], authenticator: PlainEmail) = ???
  }

  val AuthRequestHandler = SecuredRequestHandler(PlainEmailAuth)

  /**
    * замена tsec.authentication.TSecAuthService с дефолтными настройками
    */
  object AuthService {
    import cats.Monad
    import tsec.authorization.Authorization

    def apply[I, A, F[_]](
      pf: PartialFunction[SecuredRequest[F, I, A], F[Response[F]]]
    )(implicit F: Monad[F]): TSecAuthService[I, A, F] = TSecAuthService.apply(pf)

    def withAuthorization[I, A, F[_]](auth: Authorization[F, I, A])(
      pf: PartialFunction[SecuredRequest[F, I, A], F[Response[F]]]
    )(implicit F: Monad[F]): TSecAuthService[I, A, F] =
      TSecAuthService.withAuthorizationHandler(auth)(pf, defaultOnNotAuthorized[F, I, A])

    // Базовая реализация сразу возвращает Unauthorized, но это не правильно. Т.к. совокупное АПИ собирается из разных частей
    // (что-то доступно одним, что-то другим) и в произвольном порядке, то если нам не доступна какая-то часть, то надо ее
    // просто пропустить и поискать обработчик урл в других частях.
    // Т.о. если роль у чела другая, чем требуется для этого набора обработчиков, то для него этих урл как будто не существует
    // (что логично)
    def defaultOnNotAuthorized[F[_], I, A](
      unused: SecuredRequest[F, I, A]
    )(implicit F: Monad[F]): OptionT[F, Response[F]] =
      OptionT(F.pure(None))
  }


  /**
    * Роли пользователей
    */
  sealed case class Role(roleRepr: String)

  object Role extends SimpleAuthEnum[Role, String] {
    val HR: Role = Role("HR")
    val Other: Role = Role("Other")

    //implicit val E: Eq[Role] = Eq.fromUniversalEquals[Role]

    def getRepr(t: Role): String = t.roleRepr

    protected val values: AuthGroup[Role] = AuthGroup(HR, Other)
  }

  private[api] implicit val authorizationInfo: AuthorizationInfo[IO, Role, User] = new AuthorizationInfo[IO, Role, User]() {
    // список емайл, которые принадлежат пользователям с ролью HR
    private val hrEmails = {
      cfg.getString("roles.hr").split(',').map(_.trim.toLowerCase).toSet
    }

    override def fetchInfo(u: User): IO[Role] = IO.pure(
      if(hrEmails.contains(u.email)) Role.HR else Role.Other
    )
  }

  val HrOnly = BasicRBAC[IO, Role, User, PlainEmail](Role.HR)

}

