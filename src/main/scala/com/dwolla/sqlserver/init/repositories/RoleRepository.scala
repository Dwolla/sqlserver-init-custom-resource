package com.dwolla.sqlserver.init
package repositories

import cats.ApplicativeThrow
import cats.effect.std.Dispatcher
import cats.syntax.all._
import cats.tagless.Derive
import cats.tagless.aop.Instrument
import doobie._
import doobie.implicits._
import eu.timepit.refined.refineV
import org.typelevel.log4cats.Logger

trait RoleRepository[F[_]] {
  def addUserToRole(username: Username, role: RoleName, database: Database): F[Unit]
  def removeUserFromRole(username: Username, role: RoleName, database: Database): F[Unit]
}

object RoleRepository {
  implicit val RoleRepositoryInstrument: Instrument[RoleRepository] = Derive.instrument

  def roleNameForDatabase[F[_] : ApplicativeThrow](database: Database): F[RoleName] =
    refineV[SqlServerUserPredicate](database.value + "_role")
      .map(RoleName(_))
      .leftMap(new RuntimeException(_))
      .liftTo[F]

  def apply[F[_] : Logger : Dispatcher](implicit logHandler: LogHandler): RoleRepository[ConnectionIO] = new RoleRepository[ConnectionIO] {
    override def addUserToRole(username: Username, role: RoleName, database: Database): ConnectionIO[Unit] =
      RoleQueries.grantRole(username, role, database)
        .run
        .flatTap { completion =>
          Logger[ConnectionIO].info(s"added $username to role $role with status $completion")
        }
        .void

    override def removeUserFromRole(username: Username, role: RoleName, database: Database): ConnectionIO[Unit] =
      RoleQueries.revokeRole(username, role, database)
        .run
        .flatTap { completion =>
          Logger[ConnectionIO].info(s"revoked role $role from $username with status $completion")
        }
        .void
        .recoverUndefinedAs(())
    }
}

object RoleQueries {
  def grantRole(userName: Username,
                role: RoleName,
                database: Database)
               (implicit logHandler: LogHandler): Update0 =
    (fr"USE " ++ Fragment.const(database.value) ++ fr"; ALTER ROLE" ++ Fragment.const(role.value) ++ fr"ADD MEMBER" ++ Fragment.const(userName.value))
      .update

  def revokeRole(userName: Username,
                 role: RoleName,
                 database: Database)
                (implicit logHandler: LogHandler): Update0 =
    (fr"USE " ++ Fragment.const(database.value) ++ fr"; ALTER ROLE" ++ Fragment.const(role.value) ++ fr"DROP MEMBER" ++ Fragment.const(userName.value))
      .update
}
