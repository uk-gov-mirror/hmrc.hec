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

import cats.instances.future.*
import com.google.inject.{Inject, Singleton}
import play.api.Configuration
import play.api.libs.json.*
import play.api.mvc.*
import uk.gov.hmrc.hec.controllers.actions.{GGAuthenticateAction, GGOrStrideAuthenticateAction}
import uk.gov.hmrc.hec.models.SaveEmailAddressRequest
import uk.gov.hmrc.hec.models.hecTaxCheck.HECTaxCheckData
import uk.gov.hmrc.hec.models.taxCheckMatch.HECTaxCheckMatchRequest
import uk.gov.hmrc.hec.services.TaxCheckService
import uk.gov.hmrc.hec.util.Logging
import uk.gov.hmrc.hec.util.Logging.LoggerOps
import uk.gov.hmrc.internalauth.client.{BackendAuthComponents, IAAction, Predicate, Resource, ResourceLocation, ResourceType}
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendController

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class TaxCheckController @Inject() (
  taxCheckService: TaxCheckService,
  authenticateGG: GGAuthenticateAction,
  authenticatedGGOrStride: GGOrStrideAuthenticateAction,
  config: Configuration,
  auth: BackendAuthComponents,
  cc: ControllerComponents
)(implicit ec: ExecutionContext)
    extends BackendController(cc)
    with Logging {

  val internalAuthEnabled: Boolean = config.get[Boolean]("internal-auth.enabled")

  val internalAuthPermission: Predicate.Permission = Predicate.Permission(
    resource = Resource(
      resourceType = ResourceType("hec"),
      resourceLocation = ResourceLocation("hec/match-tax-check")
    ),
    action = IAAction("READ")
  )

  val saveTaxCheck: Action[JsValue] = authenticatedGGOrStride(parse.json).async { implicit request =>
    Json.fromJson[HECTaxCheckData](request.body) match {
      case JsSuccess(taxCheckData, _) =>
        taxCheckService
          .saveTaxCheck(taxCheckData)
          .fold(
            { e =>
              logger.warn("[TaxCheckController][saveTaxCheck] Could not store tax check", e)
              InternalServerError
            },
            taxCheck => Created(Json.toJson(taxCheck))
          )

      case JsError(_) =>
        logger.warn("[TaxCheckController][saveTaxCheck] Could not parse JSON")
        Future.successful(BadRequest)

    }

  }

  val matchTaxCheck: Action[JsValue] =
    (if (internalAuthEnabled) auth.authorizedAction(internalAuthPermission).apply(parse.json)
     else Action(parse.json)).async { implicit request =>
      Json.fromJson[HECTaxCheckMatchRequest](request.body) match {
        case JsSuccess(matchRequest, _) =>
          taxCheckService
            .matchTaxCheck(matchRequest)
            .fold(
              { e =>
                logger.warn("[TaxCheckController][matchTaxCheck] Could not match tax check", e)
                InternalServerError
              },
              result => Ok(Json.toJson(result))
            )

        case JsError(_) =>
          logger.warn("[TaxCheckController][matchTaxCheck] Could not parse JSON")
          Future.successful(BadRequest)
      }
    }

  val getUnexpiredTaxCheckCodes: Action[AnyContent] = authenticateGG.async { implicit request =>
    taxCheckService
      .getUnexpiredTaxCheckCodes(request.ggCredId)
      .fold(
        { e =>
          logger.warn("[TaxCheckController][getUnexpiredTaxCheckCodes] Error while fetching tax check codes", e)
          InternalServerError
        },
        result => Ok(Json.toJson(result))
      )
  }

  val saveEmailAddress: Action[JsValue] = authenticateGG.async(parse.json) { implicit request =>
    Json.fromJson[SaveEmailAddressRequest](request.body) match {
      case JsSuccess(saveEmailAddressRequest, _) =>
        taxCheckService
          .saveEmailAddress(saveEmailAddressRequest)
          .fold(
            { e =>
              logger.warn("[TaxCheckController][saveEmailAddress] Could not save email address", e)
              InternalServerError
            },
            {
              case None    => NotFound
              case Some(_) => Ok
            }
          )

      case JsError(_) =>
        logger.warn("[TaxCheckController][saveEmailAddress] Could not parse JSON")
        BadRequest
    }
  }

}
