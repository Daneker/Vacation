package kz.btsd.intranet.vacation
import cats.data.{Kleisli, OptionT}
import cats.effect.IO
import cats.syntax.semigroupk._
import io.circe.syntax._
import io.circe.{Json, JsonObject}
import kz.btsd.intranet.vacation.api.security.{AuthRequestHandler, AuthService}
import org.http4s.circe._
import org.http4s.dsl.impl.QueryParamDecoderMatcher
import org.http4s.dsl.io._
import org.http4s.{MessageFailure, QueryParamDecoder, Response}

package object api {
  val routes = AuthRequestHandler.liftService(
    CommonErrorsHandler(
      VacationRequestApi.services <+> VacationRequestApi.hrServices <+> UserApi.services
    )
  )

  /**
    * берет параметр из query string и приводит его к нужному типу. Если параметр отсутствует или есть ошибки при конвертаци,
    * то берется значение по умолчанию
    */
  abstract class FallbackQueryParamDecoderMatcher[T: QueryParamDecoder](name: String, fallback: T) {
    private val baseMatcher = new QueryParamDecoderMatcher[T](name){}

    def unapply(params: Map[String, collection.Seq[String]]): Some[T] = {
      Some(baseMatcher.unapply(params).getOrElse(fallback))
    }
  }

  def errorResponse(err: Json): IO[Response[IO]] = BadRequest(JsonObject(("error", err)).asJson)


  /**
    * каркас middleware для обработки типовых ошибок
    */
  abstract class HttpErrorHandler(handlerFn: PartialFunction[Throwable, IO[Response[IO]]]) {
    import cats.implicits._
    def apply(service: AuthService): AuthService = Kleisli { req =>
      val recoverFn = handlerFn.andThen(_.map(Option(_)))
      OptionT {
        service(req).value.recoverWith(recoverFn)
      }
    }
  }

  /**
    * обработка типовых ошибок
    */
  private object CommonErrorsHandler extends HttpErrorHandler({
    case _: IllegalArgumentException => BadRequest()
    case _: MessageFailure => BadRequest() // ошибки парсинга данных пост-запроса
  })

}