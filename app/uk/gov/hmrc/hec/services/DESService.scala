/*
 * Copyright 2024 HM Revenue & Customs
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package uk.gov.hmrc.hec.services

import cats.data.EitherT
import cats.implicits.*
import com.google.inject.{ImplementedBy, Inject, Singleton}
import play.api.http.Status.{BAD_REQUEST, NOT_FOUND, OK}
import play.api.libs.json.{Json, Reads}
import uk.gov.hmrc.hec.connectors.DESConnector
import uk.gov.hmrc.hec.models.Error
import uk.gov.hmrc.hec.models.ids.{CRN, CTUTR}
import uk.gov.hmrc.hec.services.DESService.{BackendError, DESError, DataNotFoundError, InvalidCRNError}
import uk.gov.hmrc.hec.services.DESServiceImpl.*
import uk.gov.hmrc.hec.util.Logging
import uk.gov.hmrc.http.{HeaderCarrier, HttpResponse}

import scala.concurrent.{ExecutionContext, Future}

trait DESService {

  def getCtutr(crn: CRN)(implicit hc: HeaderCarrier): EitherT[Future, DESError, CTUTR]

}

object DESService {
  sealed trait DESError extends Product with Serializable

  final case class DataNotFoundError(msg: String) extends DESError
  final case class InvalidCRNError(msg: String) extends DESError
  final case class BackendError(error: Error) extends DESError
}

@Singleton
class DESServiceImpl @Inject() (
  DESConnector: DESConnector
)(implicit ec: ExecutionContext)
    extends DESService
    with Logging {
  import uk.gov.hmrc.hec.util.HttpResponseOps.*

  private def handleErrorPath(httpResponse: HttpResponse) = {
    val responseError = s"Response to get CTUTR came back with status ${httpResponse.status}"
    httpResponse.parseJSON[GetCtutrFailure] match {
      case Left(_)        => Left(BackendError(Error(s"$responseError; could not parse body")))
      case Right(failure) =>
        val errorMsg = s"$responseError - $failure"
        (httpResponse.status, failure.code) match {
          case (NOT_FOUND, "NOT_FOUND")     => Left(DataNotFoundError(errorMsg))
          case (BAD_REQUEST, "INVALID_CRN") => Left(InvalidCRNError(errorMsg))
          case _                            => Left(BackendError(Error(errorMsg)))
        }
    }
  }

  override def getCtutr(crn: CRN)(implicit hc: HeaderCarrier): EitherT[Future, DESError, CTUTR] =
    DESConnector
      .getCtutr(crn)
      .leftMap(uk.gov.hmrc.hec.services.DESService.BackendError.apply)
      .subflatMap { httpResponse =>
        logger.info(
          s"[DESService][getCtutr] Fetch CTUTR API returned correlationId = ${httpResponse.header("CorrelationId")}"
        )
        if (httpResponse.status === OK) {
          httpResponse
            .parseJSON[GetCtutrSuccess]
            .leftMap(e => BackendError(Error(e)))
            .flatMap(response =>
              Either
                .fromOption(
                  CTUTR.fromString(response.CTUTR),
                  BackendError(Error(s"Invalid CTUTR returned - ${response.CTUTR}"))
                )
            )
        } else {
          handleErrorPath(httpResponse)
        }
      }
}

object DESServiceImpl {
  final case class GetCtutrSuccess(
    CTUTR: String
  )
  implicit val getCtutrSuccessReads: Reads[GetCtutrSuccess] = Json.reads

  final case class GetCtutrFailure(
    code: String,
    reason: String
  )
  implicit val getCtutrFailureReads: Reads[GetCtutrFailure] = Json.reads
}
