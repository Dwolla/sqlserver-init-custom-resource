package com.dwolla.sqlserver.init

import cats.effect._
import eu.timepit.refined.auto._
import feral.lambda.cloudformation._
import feral.lambda.{LambdaEnv, TestContext}
import org.http4s.syntax.all._

/**
 * Start a SqlServer docker container first:
 * {{{
 *docker run \
 *  --detach \
 *  --rm \
 *  --interactive \
 *  --tty \
 *  --name sqlserver \
 *  --env MSSQL_SA_PASSWORD=Password123 \
 *  --env MEM_LIMIT=2048 \
 *  --env ACCEPT_EULA=Y \
 *  --publish 1433:1433 \
 *  mcr.microsoft.com/azure-sql-edge:latest
 * }}}
 */
object TestRunner extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    new SqlServerDatabaseInitHandler().handler.use { handler =>
      val input =
        CloudFormationCustomResourceRequest(
          CloudFormationRequestType.DeleteRequest,
          uri"https://webhook.site/", // TODO go to webhook.site and generate a new URL for catching the responses. Don't use this for anything sensitive!
          StackId("stack-id"),
          RequestId("request-id"),
          ResourceType("Custom::SqlServerDatabase"),
          LogicalResourceId("Test"),
          None,
          DatabaseMetadata(
            Host("localhost"),
            Port(1433),
            Database("test"),
            MasterDatabaseUsername("sa"),
            MasterDatabasePassword("Password123"),
            List.empty,
            trustServerCert = true,
          ),
          None
        )

      val env = LambdaEnv.pure[IO, CloudFormationCustomResourceRequest[DatabaseMetadata]](input, TestContext[IO])

      handler(env)
    }.as(ExitCode.Success)
}
