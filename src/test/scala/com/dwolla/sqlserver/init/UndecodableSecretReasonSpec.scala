package com.dwolla.sqlserver.init

import cats.effect._
import cats.effect.std.Dispatcher
import cats.syntax.all._
import com.dwolla.sqlserver.init.aws.SecretsManagerAlg
import doobie.LogHandler
import eu.timepit.refined.auto._
import feral.lambda.cloudformation._
import feral.lambda.{LambdaEnv, TestContext}
import io.circe.Json
import io.circe.syntax._
import munit.CatsEffectSuite
import org.http4s.client.Client
import org.http4s.syntax.all._
import org.http4s.{HttpApp, Response, Status}
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.noop.NoOpLogger
import software.amazon.awssdk.services.secretsmanager.SecretsManagerAsyncClient
import software.amazon.awssdk.services.secretsmanager.model.{GetSecretValueRequest, GetSecretValueResponse}

import java.util.concurrent.CompletableFuture

class UndecodableSecretReasonSpec extends CatsEffectSuite {
  private implicit val logger: Logger[IO] = NoOpLogger[IO]
  private implicit val logHandler: LogHandler = LogHandler.nop
  private implicit val transactorFactory: TransactorFactory[IO] = _ => InMemoryTransactor()

  private val passwordMarker = "made-up-password-marker"

  private def secretWithPassword(password: String): String =
    Json.obj(
      "database" -> "made_up_db".asJson,
      "host" -> "made-up-host.example".asJson,
      "port" -> 1433.asJson,
      "user" -> "made_up_user".asJson,
      "password" -> password.asJson,
    ).noSpaces

  // A single quote is outside the characters a generated password may hold, so this fails to decode.
  private val secretWithRejectedPassword: String = secretWithPassword(s"$passwordMarker'")

  private val secretThatIsNotJson: String = s"user=made_up_user password=$passwordMarker"

  test("a user secret that decodes is read in full") {
    SecretsManagerAlg[IO](clientReturning(secretWithPassword(passwordMarker)))
      .getSecretAs[UserConnectionInfo](SecretId("made-up-secret-id"))
      .map { info =>
        assertEquals(
          (info.database.value, info.host.value, info.port.value, info.user.value, info.password.value),
          ("made_up_db", "made-up-host.example", 1433, "made_up_user", passwordMarker)
        )
      }
  }

  // feral accepts a Create without a physical resource id, and an Update or Delete with one.
  private val requests = List(
    CloudFormationRequestType.CreateRequest -> None,
    CloudFormationRequestType.UpdateRequest -> PhysicalResourceId("made_up_db"),
    CloudFormationRequestType.DeleteRequest -> PhysicalResourceId("made_up_db"),
  )

  requests.foreach { case (requestType, physicalResourceId) =>
    test(s"$requestType: a user secret whose password does not decode is reported by secret id and field, without its value") {
      responseFor(requestType, physicalResourceId, secretWithRejectedPassword).map { response =>
        assertEquals(response.hcursor.get[String]("Status"), Right("FAILED"))
        assertEquals(
          response.hcursor.get[String]("Reason"),
          Right("Could not decode secret made-up-secret-id: the value at .password is missing or invalid")
        )
        assert(!response.noSpaces.contains(passwordMarker), "the response holds the secret's password")
      }
    }

    test(s"$requestType: a user secret that is not JSON is reported by secret id, without its contents") {
      responseFor(requestType, physicalResourceId, secretThatIsNotJson).map { response =>
        assertEquals(response.hcursor.get[String]("Status"), Right("FAILED"))
        assertEquals(
          response.hcursor.get[String]("Reason"),
          Right("Could not decode secret made-up-secret-id: the secret is not valid JSON")
        )
        assert(!response.noSpaces.contains(passwordMarker), "the response holds the secret's contents")
      }
    }
  }

  private def responseFor(requestType: CloudFormationRequestType,
                          physicalResourceId: Option[PhysicalResourceId],
                          secretString: String): IO[Json] =
    Dispatcher.parallel[IO].use { implicit dispatcher =>
      for {
        captured <- Deferred[IO, Json]
        handler = SqlServerDatabaseInitHandlerImpl[IO](SecretsManagerAlg[IO](clientReturning(secretString)))
        _ <- {
          implicit val env: LambdaEnv[IO, CloudFormationCustomResourceRequest[DatabaseMetadata]] =
            LambdaEnv.pure(request(requestType, physicalResourceId), TestContext[IO])
          CloudFormationCustomResource(capturingClient(captured), handler)
        }
        response <- captured.get
      } yield response
    }

  private def clientReturning(secretString: String): SecretsManagerAsyncClient = new SecretsManagerAsyncClient {
    override def serviceName(): String = SecretsManagerAsyncClient.SERVICE_NAME
    override def close(): Unit = ()
    override def getSecretValue(request: GetSecretValueRequest): CompletableFuture[GetSecretValueResponse] =
      CompletableFuture.completedFuture(GetSecretValueResponse.builder().secretString(secretString).build())
  }

  private def capturingClient(captured: Deferred[IO, Json]): Client[IO] =
    Client.fromHttpApp(HttpApp[IO] { req =>
      req.as[String]
        .flatMap(io.circe.parser.parse(_).liftTo[IO])
        .flatMap(captured.complete)
        .as(Response[IO](Status.Ok))
    })

  private def request(requestType: CloudFormationRequestType, physicalResourceId: Option[PhysicalResourceId]) =
    CloudFormationCustomResourceRequest(
    requestType,
    uri"https://made-up.example/response",
    StackId("made-up-stack"),
    RequestId("made-up-request"),
    ResourceType("Custom::SqlServerDatabase"),
    LogicalResourceId("MadeUpResource"),
    physicalResourceId,
    DatabaseMetadata(
      Host("made-up-host.example"),
      Port(1433),
      Database("made_up_db"),
      MasterDatabaseUsername("made_up_master"),
      MasterDatabasePassword("made-up-master-password"),
      List(SecretId("made-up-secret-id")),
      TrustServerCert("false"),
    ),
    None
  )
}
