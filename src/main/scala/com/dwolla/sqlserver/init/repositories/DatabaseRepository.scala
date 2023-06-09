package com.dwolla.sqlserver.init
package repositories

import cats.effect.std.Dispatcher
import cats.effect.{Trace => _}
import cats.syntax.all._
import cats.effect.syntax.all._
import cats.tagless.Derive
import cats.tagless.aop.Instrument
import doobie._
import doobie.implicits._
import org.typelevel.log4cats.Logger

trait DatabaseRepository[F[_]] {
  def createDatabase(db: DatabaseMetadata): F[Database]
  def removeDatabase(database: Database): F[Database]
}

object DatabaseRepository {
  implicit val DatabaseRepositoryInstrument: Instrument[DatabaseRepository] = Derive.instrument

  def apply[F[_] : Logger : Dispatcher](implicit logHandler: LogHandler): DatabaseRepository[ConnectionIO] = new DatabaseRepository[ConnectionIO] {
    private def withoutTransaction[A](p: ConnectionIO[A]): ConnectionIO[A] =
      FC.setAutoCommit(true).bracket(_ => p)(_ => FC.setAutoCommit(false))

    override def createDatabase(db: DatabaseMetadata): ConnectionIO[Database] =
      checkDatabaseExists(db)
        .ifM(createDatabase(db.name), Logger[ConnectionIO].info(s"No-op: database ${db.name} already exists"))
        .as(db.name)

    private def checkDatabaseExists(db: DatabaseMetadata): ConnectionIO[Boolean] =
      DatabaseQueries.checkDatabaseExists(db.name)
        .unique
        .flatTap { count =>
          Logger[ConnectionIO].info(s"Found $count databases matching ${db.name} on ${db.username}@${db.host}:${db.port}")
        }
        .map(_ == 0)

    private def createDatabase(database: Database): ConnectionIO[Unit] =
      withoutTransaction(DatabaseQueries.createDatabase(database).run)
        .flatTap { completion =>
          Logger[ConnectionIO].info(s"created database $database with status $completion")
        }
        .void

    override def removeDatabase(database: Database): ConnectionIO[Database] =
      withoutTransaction(DatabaseQueries.dropDatabase(database).run)
        .flatTap { completion =>
          Logger[ConnectionIO].info(s"dropped database $database with status $completion")
        }
        .as(database)
        .recoverUndefinedAs(database)
  }
}

object DatabaseQueries {
  def checkDatabaseExists(db: Database)
                         (implicit logHandler: LogHandler): Query0[Long] =
    sql"SELECT count(*) as count FROM master.sys.databases WHERE name = ${db.id.value}"
      .query[Long]

  def createDatabase(database: Database)
                    (implicit logHandler: LogHandler): Update0 =
    (fr"CREATE DATABASE" ++ quotedIdentifier(database.value))
      .update

  def dropDatabase(database: Database)
                  (implicit logHandler: LogHandler): Update0 =
    (fr"USE [master]; DROP DATABASE IF EXISTS" ++ quotedIdentifier(database.value))
      .update
}
