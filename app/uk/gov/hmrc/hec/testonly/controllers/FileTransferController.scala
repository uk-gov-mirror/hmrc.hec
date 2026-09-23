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

package uk.gov.hmrc.hec.testonly.controllers

import com.google.inject.Inject
import play.api.mvc.{Action, AnyContent, ControllerComponents}
import uk.gov.hmrc.hec.services.scheduleService.HecTaxCheckExtractionService
import uk.gov.hmrc.hec.util.Logging
import uk.gov.hmrc.hec.util.Logging.*
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendController

import scala.concurrent.ExecutionContext

class FileTransferController @Inject() (
  hecTaxCheckExtractionService: HecTaxCheckExtractionService,
  cc: ControllerComponents
)(implicit ec: ExecutionContext)
    extends BackendController(cc)
    with Logging {

  val fileTransfer: Action[AnyContent] = Action.async { _ =>
    hecTaxCheckExtractionService.lockAndProcessHecData().map {
      case None =>
        logger.info("[FileTransferController][transfer] Lock to run file extraction job not obtained")
        Conflict

      case Some(Left(e)) =>
        logger.warn(s"[FileTransferController][transfer] File extraction job failed", e)
        InternalServerError

      case Some(Right(_)) =>
        logger.info(
          s"[FileTransferController][transfer] File extraction job ran successfully for creating and storing tax checks files"
        )
        Ok

    }

  }

}
