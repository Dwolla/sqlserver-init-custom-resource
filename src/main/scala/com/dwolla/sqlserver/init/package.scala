package com.dwolla.sqlserver

import eu.timepit.refined._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.predicates.all._
import io.circe.{Decoder, Encoder}
import io.estatico.newtype.Coercible
import io.estatico.newtype.macros.newtype
import shapeless.ops.hlist
import shapeless.ops.tuple._
import shapeless.syntax.std.tuple._
import shapeless.{HList, LabelledGeneric}

package object init {
  implicit class ApplyAll[P <: Product](p: P) {
    def applyAll[A, B, O](a: A)
                         (implicit
                          cm: ConstMapper.Aux[P, A, B],
                          za: ZipApply.Aux[P, B, O],
                         ): O =
      p.zipApply(p.mapConst(a))
  }

  implicit class MigrationOps[A](a: A) {
    def migrateTo[B](implicit migration: Migration[A, B]): B =
      migration.apply(a)
  }

  implicit def genericMigration[A, B, ARepr <: HList, BRepr <: HList](implicit
                                                                      aGen: LabelledGeneric.Aux[A, ARepr],
                                                                      bGen: LabelledGeneric.Aux[B, BRepr],
                                                                      inter: hlist.Intersection.Aux[ARepr, BRepr, BRepr]
                                                                     ): Migration[A, B] =
    a => bGen.from(inter.apply(aGen.to(a)))

  type IdentifierCharacterPredicate = MatchesRegex[W.`"^[A-Za-z][A-Za-z0-9_]*$"`.T]
  type SqlIdentifierPredicate = IdentifierCharacterPredicate And Size[Interval.Closed[W.`1`.T, W.`27`.T]]
  type SqlIdentifier = String Refined SqlIdentifierPredicate
  type SqlServerUserPredicate = IdentifierCharacterPredicate And Size[Interval.Closed[W.`1`.T, W.`32`.T]]
  type SqlServerUser = String Refined SqlServerUserPredicate
  val GeneratedPasswordRegex = """^[-A-Za-z0-9!"#$%&()*+,./:<=>?@\[\]^_{|}~]+$"""
  type GeneratedPasswordPredicate = MatchesRegex[GeneratedPasswordRegex.type] And Size[Interval.Closed[W.`8`.T, W.`128`.T]]
  type GeneratedPassword = String Refined GeneratedPasswordPredicate
  implicit def coercibleDecoder[A, B](implicit ev: Coercible[Decoder[A], Decoder[B]], d: Decoder[A]): Decoder[B] = ev(d)
  implicit def coercibleEncoder[A, B](implicit ev: Coercible[Encoder[A], Encoder[B]], e: Encoder[A]): Encoder[B] = ev(e)

  @newtype case class MasterDatabaseUsername(id: SqlServerUser) {
    def value: String = id.value
  }
  @newtype case class MasterDatabasePassword(value: String)
  @newtype case class SecretId(value: String)

  @newtype case class Host(value: String)
  @newtype case class Port(value: Int)
  @newtype case class TrustServerCert(value: String)

  @newtype case class Username(id: SqlServerUser) {
    def value: String = id.value
  }
  @newtype case class Password(id: GeneratedPassword) {
    def value: String = id.value
  }
  @newtype case class Database(id: SqlIdentifier) {
    def value: String = id.value
  }
  @newtype case class RoleName(id: SqlServerUser) {
    def value: String = id.value
  }
}

package init {
  trait Migration[A, B] {
    def apply(a: A): B
  }
}