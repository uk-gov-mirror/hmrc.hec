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

package uk.gov.hmrc.hec.controllers

import cats.data.Validated.*
import cats.implicits.*
import com.google.inject.{Inject, Singleton}
import play.api.libs.json.Json
import play.api.mvc.{Action, AnyContent, ControllerComponents}
import uk.gov.hmrc.hec.controllers.actions.GGAuthenticateAction
import uk.gov.hmrc.hec.models.ids.CRN
import uk.gov.hmrc.hec.services.DESService
import uk.gov.hmrc.hec.services.DESService.{BackendError, DESError, DataNotFoundError, InvalidCRNError}
import uk.gov.hmrc.hec.util.Logging
import uk.gov.hmrc.hec.util.Logging.LoggerOps
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class DESController @Inject() (
  DESService: DESService,
  authenticateGG: GGAuthenticateAction,
  cc: ControllerComponents
)(implicit ec: ExecutionContext)
    extends BackendController(cc)
    with Logging {

  private def handleError(e: DESError) = e match {
    case DataNotFoundError(msg) =>
      logger.warn(s"[DESController][handleError] $msg")
      NotFound
    case InvalidCRNError(msg)   =>
      logger.warn(s"[DESController][handleError] Invalid CRN - $msg")
      BadRequest("Invalid CRN from DES")
    case BackendError(e)        =>
      logger.warn("[DESController][handleError] Could not fetch CTUTR", e)
      InternalServerError
  }

  /** Fetch CTUTR for a company using the CRN
    * @param crn
    *   Company number
    * @return
    *   CTUTR
    */
  def getCtutr(crn: String): Action[AnyContent] = authenticateGG.async { implicit request =>
    val crnValidation = CRN.fromString(crn).toValidNel("Invalid CRN")

    crnValidation match {
      case Valid(crn) =>
        DESService
          .getCtutr(crn)
          .fold(
            handleError,
            ctutr => Ok(Json.obj("ctutr" -> ctutr))
          )

      case Invalid(e) => Future.successful(BadRequest(e.toList.mkString("; ")))
    }
  }
}
