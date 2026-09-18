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

import com.google.inject.{Inject, Singleton}
import play.api.Configuration
import uk.gov.hmrc.hec.repos.HECTaxCheckStore
import uk.gov.hmrc.hec.services.MongoLockService
import uk.gov.hmrc.hec.util.Logging
import uk.gov.hmrc.hec.util.Logging.*

import java.time.ZonedDateTime
import scala.concurrent.Future
import scala.util.{Failure, Success}

@Singleton
class ResetTaxChecksSentFlagService @Inject() (
  config: Configuration,
  mongoLockService: MongoLockService,
  taxCheckStore: HECTaxCheckStore
)(implicit ex: HECTaxCheckExtractionContext)
    extends Logging {

  private val resetSentFlagsOnStart: Boolean =
    config.get[Boolean]("hec-file-extraction-details.reset-sent-flags.enabled")

  private val resetTaxChecksCreatedOnOrAfter: ZonedDateTime =
    ZonedDateTime.parse(
      config.get[String]("hec-file-extraction-details.reset-sent-flags.reset-tax-checks-created-on-or-after")
    )

  private val lockId: String = "reset-tax-check-sent-flags"

  if (resetSentFlagsOnStart) {
    resetSentFlags().onComplete {
      case Failure(e)       =>
        logger.warn(
          s"[ResetTaxChecksSentFlagService][resetSentFlags] Could not run job reset 'sent' flags for tax check data: ${e.getMessage}"
        )
      case Success(None)    =>
        logger.info(
          "[ResetTaxChecksSentFlagService][resetSentFlags] Could not get lock to reset 'sent' flag for tax check data"
        )
      case Success(Some(_)) =>
        logger.info(
          "[ResetTaxChecksSentFlagService][resetSentFlags] Job to reset 'sent' flag for tax check data complete"
        )
    }
  } else {
    logger.info("[ResetTaxChecksSentFlagService][resetSentFlags] Reset of 'sent' flag for tax check data not enabled")
  }

  private def resetSentFlags(): Future[Option[Unit]] =
    mongoLockService.withLock(
      lockId,
      () => {
        logger.info(
          s"[ResetTaxChecksSentFlagService][resetSentFlags] Resetting 'sent' flag for all tax checks created on or after $resetTaxChecksCreatedOnOrAfter"
        )
        taxCheckStore
          .resetTaxCheckIsExtractedFlag(resetTaxChecksCreatedOnOrAfter)
          .fold(
            e =>
              logger.warn(
                "[ResetTaxChecksSentFlagService][resetSentFlags] Could not reset 'sent' flags for tax check data",
                e
              ),
            _ =>
              logger.info(
                "[ResetTaxChecksSentFlagService][resetSentFlags] Successfully reset 'sent' flags for tax check data"
              )
          )
      }
    )

}
