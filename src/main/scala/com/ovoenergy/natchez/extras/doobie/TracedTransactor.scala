/*
 * Copyright 2018 OVO Energy
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.ovoenergy.natchez.extras.doobie

import cats.data.Kleisli
import cats.effect.Async
import cats.implicits.catsSyntaxFlatMapOps
import com.ovoenergy.natchez.extras.core.Config
import com.ovoenergy.natchez.extras.core.Config.ServiceAndResource
import doobie.WeakAsync
import doobie.free.KleisliInterpreter
import doobie.util.transactor.Transactor
import natchez.{Span, Trace}

import java.sql.{Connection, PreparedStatement, ResultSet}

object TracedTransactor {
  private val DefaultResourceName = "db.execute"

  type Traced[F[_], A] = Kleisli[F, Span[F], A]
  def apply[F[_]: Async](
                          service: String,
                          transactor: Transactor[F]
                        ): Transactor[Traced[F, *]] = {
    val kleisliTransactor = transactor
      .mapK(Kleisli.liftK[F, Span[F]])(implicitly, Async.asyncForKleisli(implicitly))
    trace(ServiceAndResource(s"$service-db", DefaultResourceName), kleisliTransactor)
  }

  private def formatQuery(q: String): String =
    q.replace("\n", " ").replaceAll("\\s+", " ").trim()

  def trace[F[_]: Trace: Async](
                                 config: Config,
                                 transactor: Transactor[F]
                               ): Transactor[F] =
    transactor
      .copy(
        interpret0 = createInterpreter(config, Async[F]).ConnectionInterpreter
      )

  private def createInterpreter[F[_]: Trace](config: Config, F: Async[F]): KleisliInterpreter[F] = {
    new KleisliInterpreter[F] {
      implicit val asyncM: WeakAsync[F] =
        WeakAsync.doobieWeakAsyncForAsync(F)

      override lazy val PreparedStatementInterpreter: PreparedStatementInterpreter =
        new PreparedStatementInterpreter {

          type TracedOp[A] = Kleisli[F, PreparedStatement, A] //PreparedStatement => F[A]

          def runTraced[A](f: TracedOp[A]): TracedOp[A] =
            Kleisli {
              case TracedStatement(p, sql) =>
                Trace[F].span(config.fullyQualifiedSpanName(formatQuery(sql)))(
                  Trace[F].put("span.type" -> "db") >> f(p)
                )
              case a =>
                f(a)
            }

          override val executeBatch: TracedOp[Array[Int]] =
            runTraced(super.executeBatch)

          override val executeLargeBatch: TracedOp[Array[Long]] =
            runTraced(super.executeLargeBatch)

          override val execute: TracedOp[Boolean] =
            runTraced(super.execute)

          override val executeUpdate: TracedOp[Int] =
            runTraced(super.executeUpdate)

          override val executeQuery: TracedOp[ResultSet] =
            runTraced(super.executeQuery)
        }

      override lazy val ConnectionInterpreter: ConnectionInterpreter =
        new ConnectionInterpreter {
          override def prepareStatement(a: String): Kleisli[F, Connection, PreparedStatement] =
            super.prepareStatement(a).map(TracedStatement(_, a): PreparedStatement)
        }
    }
  }
}
