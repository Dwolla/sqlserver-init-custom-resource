package com.dwolla.sqlserver.init

import cats.effect._
import doobie.util.transactor.Transactor

import java.lang.reflect.{InvocationHandler, Method, Proxy}
import java.sql.Connection

/**
 * A doobie `Transactor` over a JDBC connection that talks to no database, for tests whose
 * programs never reach the database: every call on the connection does nothing and returns
 * the type's default value.
 */
object InMemoryTransactor {
  def apply(): Transactor[IO] =
    Transactor.fromConnection[IO](connectionThatDoesNothing)

  private val doNothing: InvocationHandler = (_: Any, method: Method, _: Array[AnyRef]) =>
    method.getReturnType match {
      case java.lang.Boolean.TYPE => java.lang.Boolean.FALSE
      case java.lang.Integer.TYPE => Integer.valueOf(0)
      case _ => null
    }

  private def connectionThatDoesNothing: Connection =
    Proxy.newProxyInstance(getClass.getClassLoader, Array(classOf[Connection]), doNothing).asInstanceOf[Connection]
}
