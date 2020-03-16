package kz.btsd.intranet.vacation.api

import cats.effect.IO
import io.circe.Json
import kz.btsd.intranet.vacation.Server
import kz.btsd.intranet.vacation.api.security.`X-User-Email`
import org.http4s
import org.http4s.{EntityDecoder, Header, Method, Request, Response, Status, Uri}
import org.http4s.circe.jsonOf

/** Утилиты для тестов апи */
object SpecUtils {

  implicit val jsonDecoder:EntityDecoder[IO, Json] = jsonOf[IO, Json]

  val email = "daneker.nurgaliyeva@btsdigital.kz"

  def check[A](actual: IO[Response[IO]], expectedStatus: Status = Status.Ok, expectedBody: Option[A] = None, printResult: Boolean = false)(
    implicit ev: EntityDecoder[IO, A]
  ): Boolean = {
    val actualResp = actual.unsafeRunSync
    if(printResult) println(actualResp.status)
    val statusCheck = actualResp.status == expectedStatus
    val bodyCheck = expectedBody.fold[Boolean](
      true)(
      expected => {
        val b = actualResp.as[A].unsafeRunSync
        if(printResult) println(b)
        b == expected
      }
    )
    statusCheck && bodyCheck
  }

  def runReq(method: Method = Method.GET, path: String, query: http4s.Query = http4s.Query.empty, authHeader: Header =
  Header(`X-User-Email`.toString(), email),
             mapFn: Request[IO] => Request[IO] = identity): IO[Response[IO]] = {
    var req = Request[IO](method = method,
      uri = Uri(path = path, query = query))
      .withHeaders(authHeader)

    req = mapFn(req)
    Server.httpApp.run(req)
  }
}
