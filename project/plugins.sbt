addSbtPlugin("io.github.davidgregory084" % "sbt-tpolecat" % "0.4.2")
addSbtPlugin("com.github.sbt" % "sbt-ci-release" % "1.5.12")
addSbtPlugin("com.github.sbt" % "sbt-native-packager" % "1.9.16")
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.11.0")
addSbtPlugin("org.jmotor.sbt" % "sbt-dependency-updates" % "1.2.7")
libraryDependencies ++= Seq(
  "com.comcast" %% "ip4s-core" % "3.1.3",
  "org.typelevel" %% "cats-effect" % "3.3.12",
  "org.typelevel" %% "log4cats-core" % "2.3.1",
)
