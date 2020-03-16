package kz.btsd.intranet.vacation.api

import cats.effect.IO
import cats.syntax.either._
import io.circe.generic.extras.Configuration
import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json, JsonObject}
import kz.btsd.intranet.vacation.model.{VacationRequest, VacationRequestStatus, VacationType}
import org.http4s.EntityEncoder
import org.http4s.circe._



object CommonJsonCodecs {
  implicit val snakeCaseConfig: Configuration = Configuration.default.withSnakeCaseMemberNames.withDefaults

  implicit val decodeVacationType: Decoder[VacationType.`type`] = Decoder.decodeString.emap { str =>
    Either.catchNonFatal(VacationType.withName(str)).leftMap(_ => str)
  }
  implicit val encodeVacationType: Encoder[VacationType.`type`] = Encoder.encodeString.contramap[VacationType.`type`](_.toString)

  implicit val encodeVacationRequestStatus: Encoder[VacationRequestStatus.status] = Encoder.encodeString.contramap[VacationRequestStatus.status](_.toString)

  // for create...
  implicit val encodeVacationRequest: Encoder[VacationRequest] = (vr: VacationRequest) => {
    var res = JsonObject(
      ("id", vr.id.asJson),
      ("type", encodeVacationType(vr.`type`)),
      ("start_date", vr.startDate.asJson),
      ("end_date", vr.endDate.asJson),
      ("status", encodeVacationRequestStatus(vr.status))
    )

    if (vr.status == VacationRequestStatus.Declined) {
      res = res.add("decline_reason", vr.declineReason.asJson)
    }
    res.asJson
  }

  implicit val vacationRequestEntityEncoder: EntityEncoder[IO, VacationRequest] = jsonEncoderOf[IO, VacationRequest]

}
