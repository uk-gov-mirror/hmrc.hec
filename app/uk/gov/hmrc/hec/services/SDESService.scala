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
import com.google.inject.{ImplementedBy, Inject, Singleton}
import play.api.http.Status.*
import play.api.libs.json.Json
import uk.gov.hmrc.hec.connectors.SDESConnector
import uk.gov.hmrc.hec.models.Error
import uk.gov.hmrc.hec.models.sdes.SDESFileNotifyRequest
import uk.gov.hmrc.hec.util.Logging
import uk.gov.hmrc.http.HeaderCarrier

import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[SDESServiceImpl])
trait SDESService {

  def fileNotify(fileNotifyRequest: SDESFileNotifyRequest)(implicit hc: HeaderCarrier): EitherT[Future, Error, Unit]
}

@Singleton
class SDESServiceImpl @Inject() (
  sdesConnector: SDESConnector
)(implicit ec: ExecutionContext)
    extends SDESService
    with Logging {

  val message: String = "Call to notify SDES came back with status::"
  override def fileNotify(fileNotifyRequest: SDESFileNotifyRequest)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, Unit] =
    sdesConnector
      .notify(fileNotifyRequest)
      .subflatMap { response =>
        response.status match {
          case NO_CONTENT =>
            logger.info(
              s"[SDESService][fileNotify] SDES has been notified of file :: ${fileNotifyRequest.file.name}. Request body was ${Json.toJson(fileNotifyRequest).toString()}. "
            )
            Right(())
          case rest       => Left(Error(s"$message $rest, ${response.body}"))
        }

      }
}
