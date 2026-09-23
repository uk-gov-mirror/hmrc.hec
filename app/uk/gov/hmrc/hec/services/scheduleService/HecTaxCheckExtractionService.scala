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

package uk.gov.hmrc.hec.services.scheduleService

import cats.data.EitherT
import cats.implicits.*
import com.google.inject.Inject
import play.api.Configuration
import uk.gov.hmrc.hec.models
import uk.gov.hmrc.hec.models.hecTaxCheck.licence.{LicenceTimeTrading, LicenceType, LicenceValidityPeriod}
import uk.gov.hmrc.hec.models.sdes.{FileAudit, FileChecksum, FileMetaData, SDESFileNotifyRequest}
import uk.gov.hmrc.hec.models.hecTaxCheck.{CorrectiveAction, HECTaxCheck, HECTaxCheckFileBodyList}
import uk.gov.hmrc.hec.models.Error
import uk.gov.hmrc.hec.services.*
import uk.gov.hmrc.hec.services.scheduleService.HecTaxCheckExtractionService.*
import uk.gov.hmrc.hec.util.{FileMapOps, UUIDGenerator}
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.objectstore.client.{Md5Hash, ObjectSummaryWithMd5}

import java.util.{Base64, UUID}
import javax.inject.Singleton
import scala.concurrent.Future

trait HecTaxCheckExtractionService {

  def lockAndProcessHecData(): Future[Option[Either[models.Error, Unit]]]

}

@Singleton
class HecTaxCheckExtractionServiceImpl @Inject() (
  taxCheckService: TaxCheckService,
  mongoLockService: MongoLockService,
  fileCreationService: FileCreationService,
  fileStoreService: FileStoreService,
  sdesService: SDESService,
  uuidGenerator: UUIDGenerator,
  config: Configuration
)(implicit
  hecTaxCheckExtractionContext: HECTaxCheckExtractionContext
) extends HecTaxCheckExtractionService
    with uk.gov.hmrc.hec.util.Logging {

  implicit val hc: HeaderCarrier = HeaderCarrier()
  val maxTaxChecksPerFile: Int   = config.get[Int]("hec-file-extraction-details.maximum-rows-per-file")

  val informationType: String           = config.get[String]("hec-file-extraction-details.file-notification-api.information-type")
  val recipientOrSender: String         =
    config.get[String]("hec-file-extraction-details.file-notification-api.recipient-or-sender")
  val fileLocationBaseUrl: String       =
    config.get[String]("hec-file-extraction-details.file-notification-api.file-location-base-url")
  val objectStoreLocationPrefix: String = s"$fileLocationBaseUrl/object-store/object/"

  val hec: String           = "HEC"
  val sdesDirectory: String = "sdes"

  val licenceType: FileDetails[LicenceType] = FileMapOps.getFileDetails[LicenceType](s"${hec}_LICENCE_TYPE")

  val licenceTimeTrading: FileDetails[LicenceTimeTrading] =
    FileMapOps.getFileDetails[LicenceTimeTrading](s"${hec}_LICENCE_TIME_TRADING")

  val licenceValidityPeriod: FileDetails[LicenceValidityPeriod] =
    FileMapOps.getFileDetails[LicenceValidityPeriod](s"${hec}_LICENCE_VALIDITY_PERIOD")

  val correctiveAction: FileDetails[CorrectiveAction] =
    FileMapOps.getFileDetails[CorrectiveAction](s"${hec}_CORRECTIVE_ACTION")

  val hecData: FileDetails[List[HECTaxCheck]] = FileMapOps.getFileDetails[List[HECTaxCheck]](s"${hec}_APPLICATION")

  val lockId: String = "hecTaxChecks"

  override def lockAndProcessHecData(): Future[Option[Either[models.Error, Unit]]] =
    mongoLockService.withLock(lockId, processHecData)

  private def processHecData()(implicit hc: HeaderCarrier): Future[Either[models.Error, Unit]] = {
    val seqNum                                      = "0001"
    val result: EitherT[Future, models.Error, Unit] =
      for {
        _ <- createAndStoreFileThenNotify(LicenceType, seqNum, licenceType.partialFileName, licenceType.dirName, true)
        _ <-
          createAndStoreFileThenNotify(
            LicenceTimeTrading,
            seqNum,
            licenceTimeTrading.partialFileName,
            licenceTimeTrading.dirName,
            true
          )
        _ <- createAndStoreFileThenNotify(
               LicenceValidityPeriod,
               seqNum,
               licenceValidityPeriod.partialFileName,
               licenceValidityPeriod.dirName,
               true
             )
        _ <- createAndStoreFileThenNotify(
               CorrectiveAction,
               seqNum,
               correctiveAction.partialFileName,
               correctiveAction.dirName,
               true
             )
        _ <- createHecFile(
               maxTaxChecksPerFile,
               hecData.partialFileName,
               hecData.dirName
             )

      } yield ()
    result.value
  }

  private def toFormattedString(i: Int) = f"$i%04d"

  private def createHecFile(
    limit: Int,
    partialFileName: String,
    dirname: String
  ): EitherT[Future, Error, Unit] = {

    def loop(
      seqNumInt: Int,
      skip: Int,
      limit: Int,
      sortBy: String,
      partialFileName: String,
      dirname: String,
      currentBatch: EitherT[Future, Error, List[HECTaxCheck]]
    ): EitherT[Future, Error, Unit] = {

      val fetchNextBatchHECTaxCheckData = for {
        hecTaxCheckList          <- currentBatch.map(_.filterNot(_.taxCheckData.filterFromFileTransfer.contains(true)))
        _                         =
          logger.info(
            s"[HecTaxCheckExtractionService][createHecFile] Processing file no :: $seqNumInt, with records:: ${hecTaxCheckList.size}}"
          )
        hecTaxCheckListNextBatch <-
          taxCheckService
            .getAllTaxCheckCodesByExtractedStatus(false, skip + limit, limit, sortBy)
            .map(_.filterNot(_.taxCheckData.filterFromFileTransfer.contains(true)))
        _                        <- if ((hecTaxCheckList.size === 0 && seqNumInt =!= 1) || seqNumInt > 9999) EitherT.pure[Future, Error](())
                                    else {
                                      createAndStoreFileThenNotify(
                                        HECTaxCheckFileBodyList(hecTaxCheckList),
                                        toFormattedString(seqNumInt),
                                        partialFileName,
                                        dirname,
                                        hecTaxCheckListNextBatch.size === 0
                                      )
                                    }
      } yield hecTaxCheckListNextBatch

      fetchNextBatchHECTaxCheckData.flatMap { hecTaxCheckList =>
        if (hecTaxCheckList.size === 0) EitherT.pure[Future, Error](())
        else loop(seqNumInt + 1, skip + limit, limit, sortBy, partialFileName, dirname, fetchNextBatchHECTaxCheckData)
      }
    }

    loop(
      1,
      0,
      limit,
      "_id",
      partialFileName,
      dirname,
      taxCheckService.getAllTaxCheckCodesByExtractedStatus(false, 0, limit, "_id")
    )
  }

  // Combining the process of creating , Storing file ,
  // updating hec tax check records with correlation Id
  // and notify the SDES about the file
  // useful when we have to create n number of files for large dataset.
  private def createAndStoreFileThenNotify[A](
    inputType: A,
    seqNum: String,
    partialFileName: String,
    dirName: String,
    isLastInSequence: Boolean
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, Unit] = {
    val uuid = uuidGenerator.generateUUID
    for {
      fileContent   <-
        EitherT.fromEither[Future](
          fileCreationService
            .createFileContent(inputType, seqNum, partialFileName, isLastInSequence)
        )
      objectSummary <- fileStoreService.storeFile(fileContent._1, fileContent._2, dirName)
      _             <- updateHecTaxCheckWithCorrelationId(inputType, uuid)
      _             <- sdesService.fileNotify(createNotifyRequest(objectSummary, fileContent._2, uuid))

    } yield ()
  }

  private def updateHecTaxCheckWithCorrelationId[A](inputType: A, uuid: UUID): EitherT[Future, Error, Unit] =
    inputType match {
      case HECTaxCheckFileBodyList(list) =>
        val updatedHecTaxCodeList = list.map(_.copy(fileCorrelationId = uuid.some))
        taxCheckService.updateAllHecTaxCheck(updatedHecTaxCodeList).map(_ => ())
      case _                             => EitherT.pure[Future, Error](())
    }

  private def createNotifyRequest(
    objSummary: ObjectSummaryWithMd5,
    fileName: String,
    uuid: UUID
  ): SDESFileNotifyRequest =
    SDESFileNotifyRequest(
      informationType,
      FileMetaData(
        recipientOrSender,
        fileName,
        s"$objectStoreLocationPrefix${objSummary.location.asUri}",
        FileChecksum(value = convertToHexString(objSummary.contentMd5)),
        objSummary.contentLength,
        List()
      ),
      FileAudit(uuid.toString)
    )

  private val toHexFormatString = "%02x"

  private def convertToHexString(md5Hash: Md5Hash): String =
    Base64.getDecoder.decode(md5Hash.value).map(toHexFormatString.format(_)).mkString

}

object HecTaxCheckExtractionService {
  final case class FileDetails[A](dirName: String, partialFileName: String)

}
