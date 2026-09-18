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

package uk.gov.hmrc.hec.controllers.actions

import com.google.inject.ImplementedBy
import play.api.mvc.*
import uk.gov.hmrc.auth.core.AuthProvider.{GovernmentGateway, PrivilegedApplication}
import uk.gov.hmrc.auth.core.*
import uk.gov.hmrc.auth.core.retrieve.{Credentials, GGCredId => AuthGGCredId, PAClientId, ~}
import uk.gov.hmrc.auth.core.retrieve.v2.Retrievals.*
import uk.gov.hmrc.hec.models.StrideOperatorDetails
import uk.gov.hmrc.hec.models.ids.{GGCredId, PID}
import uk.gov.hmrc.hec.util.Logging
import uk.gov.hmrc.play.bootstrap.backend.controller.BackendHeaderCarrierProvider

import javax.inject.Inject
import scala.annotation.nowarn
import scala.concurrent.{ExecutionContext, Future}

final case class AuthenticatedGGOrStrideRequest[+A](
  loginDetails: Either[StrideOperatorDetails, GGCredId],
  request: Request[A]
) extends WrappedRequest[A](request)

@ImplementedBy(classOf[GGOrStrideAuthenticateActionBuilder])
trait GGOrStrideAuthenticateAction extends ActionBuilder[AuthenticatedGGOrStrideRequest, AnyContent]

class GGOrStrideAuthenticateActionBuilder @Inject() (
  val authConnector: AuthConnector,
  val parser: BodyParsers.Default,
  val executionContext: ExecutionContext
) extends GGOrStrideAuthenticateAction
    with AuthorisedFunctions
    with BackendHeaderCarrierProvider
    with Logging {

  // authProviderId retrieval is deprecated but is necessary for the AuthProvider type PrivilegedApplication
  // since the recommended alternative retrieval does not work for it
  @nowarn("msg=deprecated")
  override def invokeBlock[A](
    request: Request[A],
    block: AuthenticatedGGOrStrideRequest[A] => Future[Result]
  ): Future[Result] = {
    val forbidden = Results.Forbidden("Forbidden")
    val carrier   = hc(request)

    authorised(AuthProviders(GovernmentGateway, PrivilegedApplication))
      .retrieve(credentials and allEnrolments and email) {
        case Some(Credentials(ggCredId, "GovernmentGateway")) ~ _ ~ _ =>
          block(new AuthenticatedGGOrStrideRequest[A](Right(GGCredId(ggCredId)), request))

        case Some(Credentials(pid, "PrivilegedApplication")) ~ enrolments ~ email =>
          val strideOperatorDetails = StrideOperatorDetails(
            PID(pid),
            enrolments.enrolments.map(_.key).toList,
            None,
            email
          )
          block(new AuthenticatedGGOrStrideRequest[A](Left(strideOperatorDetails), request))

        case other =>
          logger.info(s"[GGOrStrideAuthenticatedAction][invokeBlock] Found unsupported auth provider id type: $other")
          Future.successful(forbidden)
      }(carrier, executionContext)
      .recover { case _: NoActiveSession =>
        forbidden
      }(executionContext)
  }
}
