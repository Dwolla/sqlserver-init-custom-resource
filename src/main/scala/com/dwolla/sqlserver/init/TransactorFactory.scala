package com.dwolla.sqlserver.init

import cats.data._
import cats.effect.Async
import com.ovoenergy.natchez.extras.doobie.TracedTransactor
import doobie.util.transactor.Transactor
import natchez._

trait TransactorFactory[F[_]] {
  def buildTransactor(event: DatabaseMetadata): Transactor[F]
}

object TransactorFactory {
  def apply[F[_] : TransactorFactory]: TransactorFactory[F] = implicitly

  def instance[F[_] : Async]: TransactorFactory[F] =
    event => Transactor.fromDriverManager[F](
      "com.microsoft.sqlserver.jdbc.SQLServerDriver",
      sqlserverConnectionUrl(event.host, event.port, event.trustServerCert),
      event.username.value,
      event.password.value
    )

  def tracedInstance[F[_] : Async](service: String): TransactorFactory[Kleisli[F, Span[F], *]] =
    event => TracedTransactor(service, TransactorFactory.instance[F].buildTransactor(event))

  private def sqlserverConnectionUrl(host: Host,
                                   port: Port,
                                   trustServerCert: TrustServerCert): String =
    s"jdbc:sqlserver://$host:$port;encrypt=true;trustServerCertificate=$trustServerCert"

}
