package com.dwolla.sqlserver.init

import cats._
import cats.syntax.all._
import com.dwolla.sqlserver.init.repositories.ReservedWords
import eu.timepit.refined.api.{Refined, Validate}
import eu.timepit.refined.refineV
import org.scalacheck.{Arbitrary, Gen, Shrink}
import scala.jdk.CollectionConverters.IterableHasAsScala
import scala.util.Random

trait ArbitraryRefinedTypes {
  def refinedConst[A] = new PartiallyAppliedRefinedConst[A]

  implicit val shrinkSqlIdentifier: Shrink[SqlIdentifier] = Shrink.shrinkAny
  def genSqlIdentifier[F[_] : Applicative]: Gen[F[SqlIdentifier]] =
    for {
      initial <- Gen.alphaChar
      len <- Gen.chooseNum(0, 27) // see comment on SqlIdentifier
      tail <- Gen.stringOfN(len, Gen.oneOf(Gen.alphaChar, Gen.numChar, Gen.const('_')))
      identifier = s"$initial$tail"
      if !ReservedWords.contains(identifier)
      refined <- refineV[SqlIdentifierPredicate](s"$initial$tail").fold(_ => Gen.fail, Gen.const)
    } yield refined.pure[F]
  implicit val arbSqlIdentifier: Arbitrary[SqlIdentifier] = Arbitrary(genSqlIdentifier[Id])

  implicit val shrinkSqlServerUser: Shrink[SqlServerUser] = Shrink.shrinkAny
  def genSqlServerUser[F[_] : Applicative]: Gen[F[SqlServerUser]] =
    for {
      initial <- Gen.alphaChar
      len <- Gen.chooseNum(0, 63)
      tail <- Gen.stringOfN(len, Gen.oneOf(Gen.alphaChar, Gen.numChar, Gen.const('_')))
      user = s"$initial$tail"
      if !ReservedWords.contains(user)
      refined <- refineV[SqlServerUserPredicate](s"$initial$tail").fold(_ => Gen.fail, Gen.const)
    } yield refined.pure[F]
  implicit val arbSqlServerUser: Arbitrary[SqlServerUser] = Arbitrary(genSqlServerUser[Id])

  implicit val shrinkGeneratedPassword: Shrink[GeneratedPassword] = Shrink.shrinkAny
  def stringContainingAtLeastOneOfEachClass(length: Int, charClassGenerators: Gen[Char]*): Gen[String] = {
    assert(length >= charClassGenerators.length, "Output string length must be greater than or equal to the number of character classes")

    for {
      oneFromEachClass <- Gen.sequence(charClassGenerators).map(_.asScala)
      filler <- Gen.listOfN(length - charClassGenerators.length, Gen.choose(0, charClassGenerators.length - 1).flatMap(i => charClassGenerators(i)))
      chars <- Gen.long.flatMap(new Random(_).shuffle(oneFromEachClass ++ filler))
    } yield chars.mkString
  }
  def genGeneratedPassword[F[_] : Applicative]: Gen[F[GeneratedPassword]] = {
    val allowedPunctuation: List[Char] = """! " # $ % & ( ) * + , - . / : < = > ? @ [ ] ^ _ { | } ~ """.replaceAll(" ", "").toList
    val allowedPunctuationGen: Gen[Char] = Gen.oneOf(allowedPunctuation)

    for {
      length <- Gen.chooseNum(8, 128)
      password <- stringContainingAtLeastOneOfEachClass(length, Gen.alphaChar, Gen.numChar, allowedPunctuationGen)
      if !ReservedWords.contains(password)
      refined <- refineV[GeneratedPasswordPredicate](password).fold(_ => Gen.fail, Gen.const)
    } yield refined.pure[F]
  }
  implicit val arbGeneratedPassword: Arbitrary[GeneratedPassword] = Arbitrary(genGeneratedPassword[Id])
}

class PartiallyAppliedRefinedConst[A](private val unit: Unit = ()) extends AnyVal {
  def apply[T, P](t: T)
                 (implicit ev: Refined[T, P] =:= A,
                  V: Validate[T, P]): Gen[Refined[T, P]] =
    refineV[P](t).fold(_ |: Gen.fail, Gen.const)
}
