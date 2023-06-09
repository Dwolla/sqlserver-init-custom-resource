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

trait LoginRepository[F[_]] {
  def addOrUpdateLogin(userConnectionInfo: UserConnectionInfo): F[Username]
  def removeLogin(username: Username): F[Username]
}

object LoginRepository {
  implicit val LoginRepositoryInstrument: Instrument[LoginRepository] = Derive.instrument

  def usernameForDatabase(database: Database): Username =
    Username(Refined.unsafeApply(database.value))

  def apply[F[_] : Logger : Dispatcher](implicit logHandler: LogHandler): LoginRepository[ConnectionIO] = new LoginRepository[ConnectionIO] {
    override def addOrUpdateLogin(userConnectionInfo: UserConnectionInfo): ConnectionIO[Username] =
      LoginQueries.checkLoginExists(userConnectionInfo.user)
        .unique
        .flatMap {
          case 0 => Logger[ConnectionIO].info(s"Creating login ${userConnectionInfo.user}") >> LoginQueries.createLogin(userConnectionInfo.user, userConnectionInfo.password).run
          case count => Logger[ConnectionIO].info(s"Found and updating $count login named ${userConnectionInfo.user}") >> LoginQueries.updateLogin(userConnectionInfo.user, userConnectionInfo.password).run
        }
        .flatTap { completion =>
          Logger[ConnectionIO].info(s"upserted ${userConnectionInfo.user} with status $completion")
        }
        .as(userConnectionInfo.user)

    override def removeLogin(user: Username): ConnectionIO[Username] =
      LoginQueries.checkLoginExists(user)
        .unique
        .flatMap {
          case 0 => Logger[ConnectionIO].info(s"No logins found for ${user}").as(0)
          case count => Logger[ConnectionIO].info(s"Found and removing $count login named ${user}") >> LoginQueries.removeLogin(user).run
        }
        .flatTap { completion =>
          Logger[ConnectionIO].info(s"removed user $user with status $completion")
        }
        .as(user)
        .recoverUndefinedAs(user)
  }
}

object LoginQueries {
  def checkLoginExists(username: Username)
                     (implicit logHandler: LogHandler): Query0[Long] =
    sql"SELECT count(*) as count FROM sys.syslogins WHERE name = ${username.value}"
      .query[Long]

  def createLogin(username: Username,
                 password: Password)
                (implicit logHandler: LogHandler): Update0 =
    (fr"CREATE LOGIN" ++ quotedIdentifier(username.value) ++ fr0"WITH PASSWORD=N" ++ quotedPassword(password))
      .update

  def updateLogin(username: Username,
                 password: Password)
                (implicit logHandler: LogHandler): Update0 =
    (fr"ALTER LOGIN" ++ quotedIdentifier(username.value) ++ fr0"WITH PASSWORD=N" ++ quotedPassword(password))
      .update

  def removeLogin(username: Username)
                (implicit logHandler: LogHandler): Update0 =
    (fr"DROP LOGIN" ++ quotedIdentifier(username.value))
      .update

  private def quotedPassword(password: Password): Fragment =
    fr0"'" ++ Fragment.const0(escapeBackslashes(password.value)) ++ fr"'"

  private def escapeBackslashes(s: String): String =
    s.replace("""\""", """\\""")
}
