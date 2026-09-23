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
import uk.gov.hmrc.hec.actors.TimeCalculator
import uk.gov.hmrc.hec.models
import uk.gov.hmrc.hec.models.Error
import uk.gov.hmrc.hec.util.Logging
import uk.gov.hmrc.hec.util.Logging.*

import java.time.{LocalTime, ZoneId}
import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Success}

@Singleton
class HECTaxCheckScheduleService @Inject() (
  schedulerProvider: SchedulerProvider,
  timeCalculator: TimeCalculator,
  hecTaxCheckExtractionService: HecTaxCheckExtractionService,
  config: Configuration
)(implicit
  hECTaxCheckExtractionContext: HECTaxCheckExtractionContext
) extends Logging {

  val extractionTimeZone: ZoneId = ZoneId.of(config.get[String]("hec-file-extraction-details.extraction-timezone"))

  val jobStartTime: LocalTime =
    LocalTime.parse(config.get[String]("hec-file-extraction-details.extraction-time"))

  private def timeUntilNextJob(): FiniteDuration = timeCalculator.timeUntil(jobStartTime, extractionTimeZone)

  def scheduleNextJob(): Unit = {
    val _ = schedulerProvider.scheduleOnce(timeUntilNextJob())(runScheduledJob())
  }

  scheduleNextJob()

  // Run the job to put a mongo lock and perform fetch and update
  // Once that is done, call the scheduleNextJob() again to schedule the next job
  // as per the extraction time in conf
  def runScheduledJob(): Unit =
    hecTaxCheckExtractionService.lockAndProcessHecData().onComplete { result =>
      result match {
        case Success(mayBeValue) =>
          mayBeValue match {
            case Some(value) =>
              value match {
                case Left(error: models.Error) =>
                  logger.warn(s"[HECTaxCheckScheduleService][runScheduledJob] File extraction job failed", error)
                case Right(_)                  =>
                  logger.info(
                    s"[HECTaxCheckScheduleService][runScheduledJob] File extraction job ran successfully for creating and storing tax checks files"
                  )

              }
            case None        =>
              logger.info(
                "[HECTaxCheckScheduleService][runScheduledJob] File extraction job did not run as lock couldn't be obtained."
              )
          }

        case Failure(ex) =>
          logger.warn(
            s"[HECTaxCheckScheduleService][runScheduledJob] File extraction job failed with failed future",
            Error(ex)
          )
      }
      scheduleNextJob()
    }
}
