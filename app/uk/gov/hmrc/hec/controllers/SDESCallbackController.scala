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

import cats.data.EitherT
import cats.implicits.catsSyntaxEq
import com.google.inject.{Inject, Singleton}
import play.api.libs.json.{JsError, JsSuccess, JsValue, Json}
import play.api.mvc.{Action, ControllerComponents}
import uk.gov.hmrc.hec.models
import uk.gov.hmrc.hec.models.sdes.CallBackNotification
import uk.gov.hmrc.hec.models.sdes.NotificationStatus.*
import uk.gov.hmrc.hec.services.scheduleService.HECTaxCheckExtractionContext
import uk.gov.hmrc.hec.services.{FileStoreService, TaxCheckService}
import uk.gov.hmrc.hec.util.{FileMapOps, Logging}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendController

import scala.concurrent.Future

@Singleton
class SDESCallbackController @Inject() (
  fileStoreService: FileStoreService,
  taxCheckService: TaxCheckService,
  cc: ControllerComponents
)(implicit hecTaxCheckExtractionContext: HECTaxCheckExtractionContext)
    extends BackendController(cc)
    with Logging {

  val taxCheckDir = "sdes/tax-checks"

  val callback: Action[JsValue] = Action(parse.json).async { implicit request =>
    Json.fromJson[CallBackNotification](request.body) match {
      case JsSuccess(callbackNotification, _) =>
        logger.info(
          "[SDESCallbackController][callback] " +
            s"Received SDES callback for file: ${callbackNotification.filename}, " +
            s"with correlationId : ${callbackNotification.correlationID}," +
            s" status : ${callbackNotification.notification}," +
            s" and failureReason : '${callbackNotification.failureReason}'"
        )
        callbackNotification.notification match {
          case FileReady | FileReceived => Future.successful(Ok)
          case FileProcessed            =>
            deleteFileAndUpdateTaxChecks(callbackNotification.filename, callbackNotification.correlationID)
          case FileProcessingFailure    =>
            deleteFileFromObjectStore(callbackNotification.filename)
        }

      case JsError(err) =>
        logger.warn(s"[SDESCallbackController][callback] Failed to parse SDES callback notification with error:: $err")
        Future.successful(BadRequest)
    }

  }

  private def deleteFileFromObjectStore(fileName: String)(implicit
    hc: HeaderCarrier,
    hecTaxCheckExtractionContext: HECTaxCheckExtractionContext
  ) = {
    val dirName = getDirName(fileName)
    fileStoreService
      .deleteFile(fileName, dirName)
      .fold(
        { err =>
          logger.warn(
            s"[SDESCallbackController][deleteFileFromObjectStore] Failed to delete file:: $fileName from object store with error: $err"
          )
          InternalServerError
        },
        _ => Ok
      )
  }

  private def deleteFileAndUpdateTaxChecks(fileName: String, correlationId: String)(implicit
    hc: HeaderCarrier,
    hecTaxCheckExtractionContext: HECTaxCheckExtractionContext
  ) = {
    val dirName               = getDirName(fileName)
    val deleteAndUpdateResult =
      for {
        _ <- fileStoreService.deleteFile(fileName, dirName)
        _ <- if (dirName === taxCheckDir) updateHecTaxCheck(correlationId) else EitherT.pure[Future, models.Error](())
      } yield ()

    deleteAndUpdateResult.fold(
      { err =>
        logger.warn(
          s"[SDESCallbackController][deleteFileAndUpdateTaxChecks] Failure in deleting file :: $fileName and update the hec tax check list with error :: $err"
        )
        InternalServerError
      },
      _ => Ok
    )
  }

  private def updateHecTaxCheck(correlationId: String)(implicit
    hc: HeaderCarrier,
    hecTaxCheckExtractionContext: HECTaxCheckExtractionContext
  ) = for {
    hecTaxChecks          <-
      taxCheckService.getAllTaxCheckCodesByFileCorrelationId(correlationId)
    updatedHecTaxCheckList = hecTaxChecks.map(_.copy(fileCorrelationId = None, isExtracted = true))
    _                     <- taxCheckService.updateAllHecTaxCheck(updatedHecTaxCheckList)
  } yield ()

  // object store delete need dir name to delete the file but
  // call back from SDES doesn't hold this information
  // So, Identifying the directory name of the file from the map
  // which has  data like partial file name ->  dir name
  private def getDirName(filename: String) =
    FileMapOps.fileNameDirMap
      .find { case (key, _) => filename.contains(key) }
      .map(_._2)
      .getOrElse(sys.error("File doesn't belong to hec"))

}
