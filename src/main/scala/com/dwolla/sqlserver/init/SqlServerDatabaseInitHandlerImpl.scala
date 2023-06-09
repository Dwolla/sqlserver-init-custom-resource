package com.dwolla.sqlserver.init

import cats._
import cats.data._
import cats.effect.std.Dispatcher
import cats.effect.{Trace => _, _}
import cats.syntax.all._
import com.dwolla.sqlserver.init.InputValidationError.userList
import com.dwolla.sqlserver.init.SqlServerDatabaseInitHandlerImpl._
import com.dwolla.sqlserver.init.aws.{ResourceNotFoundException, SecretsManagerAlg}
import com.dwolla.sqlserver.init.repositories._
import doobie._
import doobie.implicits._
import eu.timepit.refined.{refineMV}
import feral.lambda.INothing
import feral.lambda.cloudformation._
import org.typelevel.log4cats.Logger
import retry._
import retry.RetryDetails._
import retry.syntax.all._

import scala.concurrent.duration.DurationInt
import scala.util.control.NoStackTrace

class SqlServerDatabaseInitHandlerImpl[F[_] : Temporal : Logger : TransactorFactory](secretsManagerAlg: SecretsManagerAlg[F],
                                                                                     databaseRepository: DatabaseRepository[ConnectionIO],
                                                                                     roleRepository: RoleRepository[ConnectionIO],
                                                                                     userRepository: UserRepository[ConnectionIO],
                                                                                     loginRepository: LoginRepository[ConnectionIO],
                                                                                    ) extends CloudFormationCustomResource[F, DatabaseMetadata, INothing] {

  val db_reader: RoleName = RoleName(refineMV[SqlServerUserPredicate]("db_datareader"))
  val db_writer: RoleName = RoleName(refineMV[SqlServerUserPredicate]("db_datawriter"))

  override def createResource(event: DatabaseMetadata): F[HandlerResponse[INothing]] =
    handleCreateOrUpdate(event)(createOrUpdate(_, event)).map(HandlerResponse(_, None))

  override def updateResource(event: DatabaseMetadata, physicalResourceId: PhysicalResourceId): F[HandlerResponse[INothing]] =
    handleCreateOrUpdate(event)(createOrUpdate(_, event)).map(HandlerResponse(_, None))

  override def deleteResource(event: DatabaseMetadata, physicalResourceId: PhysicalResourceId): F[HandlerResponse[INothing]] =
    for {
      usernames <- getUsernamesFromSecrets(event.secretIds, UserRepository.usernameForDatabase(event.name))
      dbId <- removeUsersFromDatabase(usernames, event.name)
                .transact(TransactorFactory[F].buildTransactor(event))
                .retryingOnAllErrors(jitteryLimitedRetries[F], logError[F])
    } yield HandlerResponse(dbId, None)

  private def createOrUpdate(userPasswords: List[UserConnectionInfo], input: DatabaseMetadata): ConnectionIO[PhysicalResourceId] = {
      for {
      db <- databaseAsPhysicalResourceId[ConnectionIO](input.name)
      _ <- databaseRepository.createDatabase(input)
      _ <- userPasswords.traverse { userPassword =>
        loginRepository.addOrUpdateLogin(userPassword) >>
          userRepository.addOrUpdateUser(userPassword) >>
          roleRepository.addUserToRole(userPassword.user, db_reader, input.name) >>
          roleRepository.addUserToRole(userPassword.user, db_writer, input.name)
      }
    } yield db
  }

  private def handleCreateOrUpdate(input: DatabaseMetadata)
                                  (f: List[UserConnectionInfo] => ConnectionIO[PhysicalResourceId]): F[PhysicalResourceId] =
    for {
      userPasswords <- input.secretIds.traverse(secretsManagerAlg.getSecretAs[UserConnectionInfo])
      _ <- List(
          ensureDatabaseConnectionInfoMatches(_, input),
          ensureNoDuplicateUsers(_),
          ensureNoIdentifiersAsReservedWords(_)
        )
        .map(_(userPasswords))
        .parSequence
        .leftMap(InputValidationException)
        .liftTo[F]
      id <- f(userPasswords).transact(TransactorFactory[F].buildTransactor(input))
    } yield id

  private def getUsernamesFromSecrets(secretIds: List[SecretId], fallback: Username): F[List[Username]] =
    secretIds.traverse { secretId =>
      secretsManagerAlg.getSecretAs[UserConnectionInfo](secretId)
        .map(_.user)
        .recoverWith {
          case ex: ResourceNotFoundException =>
            Logger[F].warn(ex)(s"could not retrieve secret ${secretId.value}, falling back to ${fallback.value}")
              .as(fallback)
        }
    }

  private def removeUsersFromDatabase(usernames: List[Username], databaseName: Database): ConnectionIO[PhysicalResourceId] =
    for {
      db <- databaseAsPhysicalResourceId[ConnectionIO](databaseName)
      _ <- usernames.traverse { user =>
        roleRepository.removeUserFromRole(user, db_reader, databaseName) >>
          roleRepository.removeUserFromRole(user, db_writer, databaseName) >>
          userRepository.removeUser(user, databaseName) >>
          loginRepository.removeLogin(user)
      }
      _ <- databaseRepository.removeDatabase(databaseName)
    } yield db

  private def ensureDatabaseConnectionInfoMatches(users: List[UserConnectionInfo], db: DatabaseMetadata): EitherNel[InputValidationError, Unit] = {
    val mismatches =
      users
        .filterNot { uci =>
          uci.database == db.name && uci.host == db.host && uci.port == db.port
        }
        .map(_.user)

    if (mismatches.isEmpty) ().rightNel
    else WrongDatabaseConnection(mismatches, db).leftNel
  }

  private def ensureNoIdentifiersAsReservedWords(users: List[UserConnectionInfo]): EitherNel[InputValidationError, Unit] = {
    val reservedIdentifiers =
      users
        .map(_.user)
        .filter(u => ReservedWords.contains(u.value))

    if (reservedIdentifiers.isEmpty) ().rightNel
    else ReservedWordAsIdentifier(reservedIdentifiers).leftNel
  }


  private def ensureNoDuplicateUsers(users: List[UserConnectionInfo]): EitherNel[InputValidationError, Unit] = {
    val duplicates: Iterable[Username] =
      users
        .groupBy(_.user)
        .filter {
          case (_, l) => l.length > 1
        }
        .keys

    if (duplicates.isEmpty) ().rightNel
    else DuplicateUsers(duplicates).leftNel
  }
}

sealed trait InputValidationError {
  val message: String
}

object InputValidationError {
  def userList(users: Iterable[Username]): String =
    users.mkString(" - ", "\n - ", "")
}

case class InputValidationException(errors: NonEmptyList[InputValidationError]) extends RuntimeException(
  errors.map(_.message).mkString_("\n")
) with NoStackTrace

case class WrongDatabaseConnection(users: List[Username], db: DatabaseMetadata) extends InputValidationError {
  val message: String =
    s"""The specified secrets contain database connection information that doesn't match the database instance being initialized:
       |
       |Expected database instance: sqlserver://${db.host}:${db.port}/${db.name}"
       |
       |Mismatched users:
       |${userList(users)}""".stripMargin
}

case class DuplicateUsers(users: Iterable[Username]) extends InputValidationError {
  val message: String =
    s"""The specified secrets refer to users that share database and usernames. Deduplicate the input and try again.
       |
       |${userList(users)}""".stripMargin
}

case class ReservedWordAsIdentifier(users: List[Username]) extends InputValidationError {
  override val message: String =
    s"""The specified secrets refer to usernames that are SQL Server reserved words. Change or remove those users from the input and try again.
       |
       |${userList(users)}""".stripMargin
}

object SqlServerDatabaseInitHandlerImpl {
  def apply[F[_] : Temporal : Logger : Dispatcher : TransactorFactory](secretsManager: SecretsManagerAlg[F])
                                                                      (implicit logHandler: LogHandler): SqlServerDatabaseInitHandlerImpl[F] =
    new SqlServerDatabaseInitHandlerImpl(
      secretsManager,
      DatabaseRepository[F],
      RoleRepository[F],
      UserRepository[F],
      LoginRepository[F],
    )

  private[SqlServerDatabaseInitHandlerImpl] def databaseAsPhysicalResourceId[F[_] : ApplicativeThrow](db: Database): F[PhysicalResourceId] =
    PhysicalResourceId(db.value).liftTo[F](new RuntimeException("Database name was invalid as Physical Resource ID"))

  private[SqlServerDatabaseInitHandlerImpl] def jitteryLimitedRetries[F[_] : Applicative] =
    RetryPolicies.fullJitter[F](5.seconds) |+| RetryPolicies.limitRetries[F](5)

  private[SqlServerDatabaseInitHandlerImpl] def logError[F[_] : Logger](err: Throwable, details: RetryDetails): F[Unit] = details match {
    case WillDelayAndRetry(nextDelay, _, _) =>
      Logger[F].warn(err)(s"Failed when removing user; retrying in $nextDelay")

    case GivingUp(totalRetries: Int, _) =>
      Logger[F].error(err)(s"Failed when removing user after $totalRetries retries")
  }
}
