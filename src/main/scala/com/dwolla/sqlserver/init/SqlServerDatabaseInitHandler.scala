package com.dwolla.sqlserver.init

import cats.data._
import cats.effect.std._
import cats.effect.syntax.all._
import cats.effect.{Trace => _, _}
import cats.tagless.FunctorK.ops.toAllFunctorKOps
import com.comcast.ip4s.{IpAddress, SocketAddress}
import com.dwolla.sqlserver.init.aws.SecretsManagerAlg
import com.dwolla.tracing._
import doobie.util.log.LogHandler
import feral.lambda.cloudformation._
import feral.lambda.{INothing, IOLambda, KernelSource, LambdaEnv, TracedHandler}
import fs2.io.net.Network
import natchez._
import natchez.http4s.NatchezMiddleware
import natchez.noop.NoopSpan
import natchez.xray.{XRay, XRayEnvironment}
import org.http4s.client.{Client, middleware}
import org.http4s.ember.client.EmberClientBuilder
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

class SqlServerDatabaseInitHandler extends IOLambda[CloudFormationCustomResourceRequest[DatabaseMetadata], INothing] {
  private implicit val logHandler = LogHandler.nop
  override def handler: Resource[IO, LambdaEnv[IO, CloudFormationCustomResourceRequest[DatabaseMetadata]] => IO[Option[INothing]]] =
    new SqlServerDatabaseInitHandlerF[IO].handler
}

class SqlServerDatabaseInitHandlerF[F[_] : Async : XRayEnvironment : Network](implicit logHandler: LogHandler) {

  private implicit def resourceXRayEnvironment: XRayEnvironment[Resource[F, *]] =
    new XRayEnvironment[Resource[F, *]] {
      override def daemonAddress: Resource[F, Option[SocketAddress[IpAddress]]] = XRayEnvironment[F].daemonAddress.toResource
      override def traceId: Resource[F, Option[String]] = XRayEnvironment[F].traceId.toResource
      override def kernelFromEnvironment: Resource[F, Kernel] = XRayEnvironment[F].kernelFromEnvironment.toResource
    }

  def handler: Resource[F, LambdaEnv[F, CloudFormationCustomResourceRequest[DatabaseMetadata]] => F[Option[INothing]]] =
    for {
      implicit0(logger: Logger[F]) <- Resource.eval(Slf4jLogger.create[F])
      implicit0(random: Random[F]) <- Resource.eval(Random.scalaUtilRandom[F])
      implicit0(dispatcher: Dispatcher[Kleisli[F, Span[F], *]]) <- Dispatcher.parallel[Kleisli[F, Span[F], *]].mapK(Kleisli.applyK(NoopSpan()))
      client <- httpClient
      entryPoint <- XRayEnvironment[Resource[F, *]].daemonAddress.flatMap {
        case Some(addr) => XRay.entryPoint(addr)
        case None => XRay.entryPoint[F]()
      }
      secretsManager <- secretsManagerResource
    } yield { implicit env: LambdaEnv[F, CloudFormationCustomResourceRequest[DatabaseMetadata]] =>
      implicit val transactorFactory: TransactorFactory[Kleisli[F, Span[F], *]] = TransactorFactory.tracedInstance[F]("SqlServerDatabaseInitHandler")

      TracedHandler(entryPoint, Kleisli { (span: Span[F]) =>
        CloudFormationCustomResource(tracedHttpClient(client, span), SqlServerDatabaseInitHandlerImpl(secretsManager)).run(span)
      })
    }

  /**
   * The XRay kernel comes from environment variables, so we don't need to extract anything from the incoming event
   */
  private implicit def kernelSource[Event]: KernelSource[Event] = KernelSource.emptyKernelSource

  protected def secretsManagerResource(implicit L: Logger[F]): Resource[F, SecretsManagerAlg[Kleisli[F, Span[F], *]]] =
    SecretsManagerAlg.resource[F].map(_.mapK(Kleisli.liftK[F, Span[F]]).withTracing)

  protected def httpClient: Resource[F, Client[F]] =
    EmberClientBuilder
      .default[F]
      .build
      .map(middleware.Logger[F](logHeaders = true, logBody = true))

  private def tracedHttpClient(client: Client[F], span: Span[F]): Client[Kleisli[F, Span[F], *]] =
    NatchezMiddleware.client(client.translate(Kleisli.liftK[F, Span[F]])(Kleisli.applyK(span)))

}
