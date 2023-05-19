package com.dwolla.sqlserver.init
package repositories

import cats.effect.std.Dispatcher
import cats.syntax.all._
import cats.tagless.Derive
import cats.tagless.aop.Instrument
import doobie._
import doobie.implicits._
import eu.timepit.refined.api.Refined
import org.typelevel.log4cats._

trait UserRepository[F[_]] {
  def addOrUpdateUser(userConnectionInfo: UserConnectionInfo): F[Username]
  def removeUser(username: Username, db: Database): F[Username]
}

object UserRepository {
  implicit val UserRepositoryInstrument: Instrument[UserRepository] = Derive.instrument

  def usernameForDatabase(database: Database): Username =
    Username(Refined.unsafeApply(database.value))

  def apply[F[_] : Logger : Dispatcher](implicit logHandler: LogHandler): UserRepository[ConnectionIO] = new UserRepository[ConnectionIO] {
    override def addOrUpdateUser(userConnectionInfo: UserConnectionInfo): ConnectionIO[Username] =
      UserQueries.checkUserExists(userConnectionInfo.user, userConnectionInfo.database)
        .unique
        .flatMap {
          case 0 => Logger[ConnectionIO].info(s"Creating user ${userConnectionInfo.user}") >> UserQueries.createUser(userConnectionInfo.user, userConnectionInfo.database).run
          case count => Logger[ConnectionIO].info(s"Found $count user named ${userConnectionInfo.user}, no user updates necessary").as(0)
        }
        .flatTap { completion =>
          Logger[ConnectionIO].info(s"upserted ${userConnectionInfo.user} with status $completion")
        }
        .as(userConnectionInfo.user)

    override def removeUser(user: Username, db: Database): ConnectionIO[Username] =
      UserQueries.removeUser(user, db)
        .run
        .flatTap { completion =>
          Logger[ConnectionIO].info(s"removed user $user with status $completion")
        }
        .as(user)
        .recoverUndefinedAs(user)
  }
}

object UserQueries {
  def checkUserExists(username: Username, database: Database)
                     (implicit logHandler: LogHandler): Query0[Long] =
    (fr"USE" ++ quotedIdentifier(database.value) ++ fr"; SELECT count(*) as count FROM sys.database_principals WHERE name = ${username.value}").query

  def createUser(username: Username, database: Database)
                (implicit logHandler: LogHandler): Update0 =
    (fr"USE" ++ quotedIdentifier(database.value) ++ fr"; CREATE USER" ++ quotedIdentifier(username.value) ++ fr"FOR LOGIN" ++ quotedIdentifier(username.value) ++ fr"WITH DEFAULT_SCHEMA=[dbo]")
      .update

  def removeUser(username: Username, database: Database)
                (implicit logHandler: LogHandler): Update0 =
    (fr"USE" ++ quotedIdentifier(database.value) ++ fr"; DROP USER IF EXISTS" ++ quotedIdentifier(username.value))
      .update
}
