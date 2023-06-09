ThisBuild / organization := "com.dwolla"
ThisBuild / description := "CloudFormation custom resource to initialize a Sql Server database with a new user"
ThisBuild / homepage := Some(url("https://github.com/Dwolla/sqlserver-init-custom-resource"))
ThisBuild / licenses += ("MIT", url("https://opensource.org/licenses/MIT"))
ThisBuild / scalaVersion := "2.13.10"
ThisBuild / scalacOptions += "-Ymacro-annotations"
ThisBuild / developers := List(
  Developer(
    "csutton",
    "Caleb Sutton",
    "csutton+sqlserver-init-custom-resource@dwolla.com",
    url("https://dwolla.com")
  ),
)
ThisBuild / startYear := Option(2021)
ThisBuild / libraryDependencies ++= Seq(
  compilerPlugin("org.typelevel" %% "kind-projector" % "0.13.2" cross CrossVersion.full),
  compilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")
)

lazy val `sqlserver-init-custom-resource` = (project in file("."))
  .settings(
    maintainer := developers.value.head.email,
    topLevelDirectory := None,
    libraryDependencies ++= {
      val natchezVersion = "0.3.1"
      val feralVersion = "0.2.2"
      val doobieVersion = "1.0.0-RC2"
      val munitVersion = "0.7.29"
      val circeVersion = "0.14.5"
      val scalacheckEffectVersion = "1.0.4"
      val log4catsVersion = "2.6.0"
      val monocleVersion = "2.1.0"
      val http4sVersion = "0.23.19"
      val awsSdkVersion = "2.17.190"
      val refinedV = "0.9.29"
      val catsRetryVersion = "3.1.0"

      Seq(
        "org.typelevel" %% "feral-lambda-cloudformation-custom-resource" % feralVersion,
        "org.tpolecat" %% "natchez-noop" % natchezVersion,
        "org.tpolecat" %% "natchez-xray" % natchezVersion,
        "org.tpolecat" %% "natchez-http4s" % "0.5.0",
        "org.typelevel" %% "cats-tagless-macros" % "0.14.0",
        "org.http4s" %% "http4s-ember-client" % http4sVersion,
        "io.circe" %% "circe-parser" % circeVersion,
        "io.circe" %% "circe-generic" % circeVersion,
        "io.circe" %% "circe-refined" % circeVersion,
        "io.estatico" %% "newtype" % "0.4.4",
        "org.typelevel" %% "log4cats-slf4j" % log4catsVersion,
        "com.amazonaws" % "aws-lambda-java-log4j2" % "1.5.1" % Runtime,
        "org.apache.logging.log4j" % "log4j-slf4j-impl" % "2.20.0" % Runtime,
        "com.chuusai" %% "shapeless" % "2.3.10",
        "com.dwolla" %% "fs2-aws-java-sdk2" % "3.0.0-RC1",
        "software.amazon.awssdk" % "secretsmanager" % awsSdkVersion,
        "org.tpolecat" %% "doobie-core" % doobieVersion,
        "org.tpolecat" %% "doobie-refined" % doobieVersion,
        "com.microsoft.sqlserver" % "mssql-jdbc" % "12.2.0.jre11" % Runtime,
        "com.ovoenergy" %% "natchez-extras-core" % "7.0.0",
        "com.github.cb372" %% "cats-retry" % catsRetryVersion,
        "org.typelevel" %% "cats-parse" % "0.3.9",
        "org.scalameta" %% "munit" % munitVersion % Test,
        "org.scalameta" %% "munit-scalacheck" % munitVersion % Test,
        "io.circe" %% "circe-literal" % circeVersion % Test,
        "org.typelevel" %% "munit-cats-effect-3" % "1.0.7" % Test,
        "org.typelevel" %% "scalacheck-effect" % scalacheckEffectVersion % Test,
        "org.typelevel" %% "scalacheck-effect-munit" % scalacheckEffectVersion % Test,
        "org.typelevel" %% "log4cats-noop" % log4catsVersion % Test,
        "io.circe" %% "circe-testing" % circeVersion % Test,
        "io.circe" %% "circe-optics" % "0.14.1" % Test,
        "com.github.julien-truffaut" %% "monocle-core" % monocleVersion % Test,
        "com.github.julien-truffaut" %% "monocle-macro" % monocleVersion % Test,
        "org.http4s" %% "http4s-dsl" % http4sVersion % Test,
        "com.eed3si9n.expecty" %% "expecty" % "0.16.0" % Test,
        "software.amazon.awssdk" % "sts" % awsSdkVersion % Test,
        "eu.timepit" %% "refined-scalacheck" % refinedV % Test,
        "org.typelevel" %% "cats-laws" % "2.9.0" % Test,
        "org.typelevel" %% "discipline-munit" % "1.0.9" % Test,
      )
    },
    addBuildInfoToConfig(Test),
    Test / testOptions ++= (Test / shouldRunIntegrationTests).value.testArguments,
  )
  .enablePlugins(
    UniversalPlugin,
    JavaAppPackaging,
    IntegrationTestsPlugin,
    BuildInfoPlugin,
    ServerlessDeployPlugin,
  )
