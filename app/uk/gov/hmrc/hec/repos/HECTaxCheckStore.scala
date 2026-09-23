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

package uk.gov.hmrc.hec.repos

import cats.data.{EitherT, OptionT}
import cats.instances.either.*
import cats.syntax.either.*
import com.google.inject.{ImplementedBy, Inject, Singleton}
import org.mongodb.scala.bson.{BsonDateTime, BsonDocument}
import org.mongodb.scala.model.Filters.gte
import org.mongodb.scala.model.*
import play.api.Configuration
import play.api.libs.json.{JsError, JsSuccess, JsonValidationError}
import uk.gov.hmrc.hec.models.Error
import uk.gov.hmrc.hec.models.hecTaxCheck.{HECTaxCheck, HECTaxCheckCode}
import uk.gov.hmrc.hec.models.ids.GGCredId
import uk.gov.hmrc.hec.util.Logging
import uk.gov.hmrc.http.HeaderCarrier
import uk.gov.hmrc.mongo.cache.{CacheIdType, CacheItem, DataKey, MongoCacheRepository}
import uk.gov.hmrc.mongo.{CurrentTimestampSupport, MongoComponent, MongoUtils}
import uk.gov.hmrc.mdc.Mdc.preservingMdc

import java.time.ZonedDateTime
import scala.concurrent.duration.*
import scala.concurrent.{ExecutionContext, Future}

@ImplementedBy(classOf[HECTaxCheckStoreImpl])
trait HECTaxCheckStore {

  def get(taxCheckCode: HECTaxCheckCode)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, Option[HECTaxCheck]]

  def getTaxCheckCodes(GGCredId: GGCredId)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, List[HECTaxCheck]]

  def store(
    taxCheck: HECTaxCheck
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, Unit]

  def delete(taxCheckCode: HECTaxCheckCode)(implicit hc: HeaderCarrier): EitherT[Future, Error, Unit]

  def deleteAll()(implicit hc: HeaderCarrier): EitherT[Future, Error, Unit]

  def getAllTaxCheckCodesByExtractedStatus(status: Boolean, skip: Int, limit: Int, sortBy: String)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, List[HECTaxCheck]]

  def getAllTaxCheckCodesByFileCorrelationId(correlationId: String)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, List[HECTaxCheck]]

  def resetTaxCheckIsExtractedFlag(createdOnOrAfter: ZonedDateTime): EitherT[Future, Error, Unit]

}

@Singleton
class HECTaxCheckStoreImpl @Inject() (
  mongo: MongoComponent,
  configuration: Configuration
)(implicit
  ec: ExecutionContext
) extends MongoCacheRepository(
      mongoComponent = mongo,
      replaceIndexes = true,
      collectionName = "hecTaxChecks",
      ttl = configuration.get[FiniteDuration]("hec-tax-check.ttl"),
      timestampSupport = new CurrentTimestampSupport(), // Provide a different one for testing
      cacheIdType = CacheIdType.SimpleCacheId // Here, CacheId to be represented with `String`
    )
    with HECTaxCheckStore
    with Logging {

  lazy val key: String                   = "hec-tax-check"
  private lazy val ggCredIdField: String = s"data.$key.taxCheckData.applicantDetails.ggCredId"
  private val isExtractedField: String   = s"data.$key.isExtracted"
  private val fileCorrelationId: String  = s"data.$key.fileCorrelationId"

  // indexes for hecTaxChecks collection
  def mongoIndexes: Seq[IndexModel] = Seq(
    IndexModel(
      Indexes.ascending(ggCredIdField),
      IndexOptions()
        .name("ggCredIdIndex")
    ),
    IndexModel(
      Indexes.ascending("isExtracted"),
      IndexOptions()
        .name("isExtractedIndex")
        .partialFilterExpression(BsonDocument("isExtracted" -> false))
    ),
    IndexModel(
      Indexes.ascending("fileCorrelationId"),
      IndexOptions()
        .name("fileCorrelationIdIndex")
    ),
    IndexModel(
      Indexes.ascending("data.hec-tax-check.fileCorrelationId"),
      IndexOptions()
        .name("dataFileCorrIndex")
    )
  )

  override def ensureIndexes(): Future[Seq[String]] =
    super.ensureIndexes().flatMap(_ => MongoUtils.ensureIndexes(collection, mongoIndexes, false))

  def get(taxCheckCode: HECTaxCheckCode)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, Option[HECTaxCheck]] =
    EitherT(
      preservingMdc {
        findById(taxCheckCode.value)
          .map { maybeCache =>
            type ErrorOr[A] = Either[Error, A]
            val response: OptionT[ErrorOr, HECTaxCheck] = for {
              cache      <- OptionT.fromOption[ErrorOr](maybeCache)
              // even if there is no data , cache returns with -> {"id" : "code1", data : {}}
              // so added a logic if the json is empty, then return None
              // but if there is, then proceed to validate json
              cacheLength = cache.data.keys.size
              data       <- OptionT.fromOption[ErrorOr](if (cacheLength == 0) None else Some(cache.data))
              result     <- OptionT.liftF[ErrorOr, HECTaxCheck](
                              (data \ key)
                                .validate[HECTaxCheck]
                                .asEither
                                .leftMap(e =>
                                  Error(
                                    s"Could not parse session data from mongo: ${e.mkString("; ")}"
                                  )
                                )
                            )
            } yield result

            response.value
          }
          .recover { case e =>
            logger.warn("[HECTaxCheckStore][get] Mongo read of tax check failed", e); Left(Error(e))
          }
      }
    )

  /** Fetch existing tax check codes for the specified GGCredId
    * @param ggCredId
    *   The government gateway ID
    * @param hc
    *   header information
    * @return
    *   A list of tax check code details
    */
  def getTaxCheckCodes(ggCredId: GGCredId)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, List[HECTaxCheck]] = find[String](ggCredIdField, ggCredId.value)

  def getAllTaxCheckCodesByExtractedStatus(isExtracted: Boolean, skip: Int, limit: Int, sortBy: String)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, List[HECTaxCheck]] =
    findByLimit[Boolean](isExtractedField, isExtracted, skip, limit, sortBy)

  def getAllTaxCheckCodesByFileCorrelationId(correlationId: String)(implicit
    hc: HeaderCarrier
  ): EitherT[Future, Error, List[HECTaxCheck]] = find[String](fileCorrelationId, correlationId)

  def store(
    taxCheck: HECTaxCheck
  )(implicit hc: HeaderCarrier): EitherT[Future, Error, Unit] =
    EitherT(
      preservingMdc {
        put[HECTaxCheck](taxCheck.taxCheckCode.value)(DataKey(key), taxCheck)
          .map(_ => Right(()))
          .recover { case e =>
            logger.warn("[HECTaxCheckStore][store] Mongo write of tax check failed", e); Left(Error(e))
          }
      }
    )

  def delete(taxCheckCode: HECTaxCheckCode)(implicit hc: HeaderCarrier): EitherT[Future, Error, Unit] =
    EitherT(
      preservingMdc {
        deleteEntity(taxCheckCode.value)
          .map(Right(_))
          .recover { case e =>
            logger.warn("[HECTaxCheckStore][delete] Mongo delete of tax check failed", e); Left(Error(e))
          }
      }
    )

  def deleteAll()(implicit hc: HeaderCarrier): EitherT[Future, Error, Unit] = EitherT(
    preservingMdc {
      collection
        .drop()
        .toFuture()
        .map(_ => ())
        .map[Either[Error, Unit]](Right(_))
        .recover { case e =>
          logger.warn("[HECTaxCheckStore][deleteAll] Mongo deleteAll of tax checks failed", e); Left(Error(e))
        }
    }
  )

  def resetTaxCheckIsExtractedFlag(createdOnOrAfter: ZonedDateTime): EitherT[Future, Error, Unit] = EitherT(
    preservingMdc {
      collection
        .updateMany(
          gte("modifiedDetails.createdAt", BsonDateTime(createdOnOrAfter.toInstant.toEpochMilli)),
          BsonDocument("{ $set: { \"data.hec-tax-check.isExtracted\": false } }")
        )
        .toFuture()
        .map[Either[Error, Unit]](_ => Right(()))
        .recover { case e =>
          logger.warn("[HECTaxCheckStore][resetTaxCheckIsExtractedFlag] Mongo reset extracted flag failed", e);
          Left(Error(e))
        }
    }
  )

  private def find[T](
    fieldName: String,
    value: T
  ): EitherT[Future, Error, List[HECTaxCheck]] =
    EitherT(
      preservingMdc {
        collection
          .find(Filters.equal[T](fieldName, value))
          .toFuture()
          .map(processCacheValues)
      }
    )

  private def findByLimit[T](
    fieldName: String,
    value: T,
    skipN: Int,
    limitN: Int,
    sortByFieldName: String
  ): EitherT[Future, Error, List[HECTaxCheck]] =
    EitherT(
      preservingMdc {
        collection
          .find(Filters.equal[T](fieldName, value))
          .sort(Sorts.ascending(sortByFieldName))
          .skip(skipN)
          .limit(limitN)
          .toFuture()
          .map(processCacheValues)
      }
    )

  private def processCacheValues(caches: Seq[CacheItem]): Either[Error, List[HECTaxCheck]] = {
    val jsons = caches
      .map(_.data)
      .toList
      .map(json => (json \ key).validate[HECTaxCheck])

    val (valid, invalid) =
      jsons.foldLeft((List.empty[HECTaxCheck], Seq.empty[JsonValidationError])) { (acc, json) =>
        val (taxChecks, errors) = acc
        json match {
          case JsSuccess(value, _)       => (value +: taxChecks, errors)
          case JsError(validationErrors) => (taxChecks, errors ++ validationErrors.flatMap(_._2))
        }
      }

    val errorStr = invalid.map(_.message).mkString("; ")
    if (invalid.nonEmpty) {
      logger.warn(s"[HECTaxCheckStore][processCacheValues] ${invalid.size} results failed json parsing - $errorStr")
    }

    Either.cond(invalid.isEmpty, valid, Error(Left(errorStr)))

  }
}
