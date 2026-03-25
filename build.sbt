ThisBuild / licenses             := Seq("ISC" -> url("https://opensource.org/licenses/ISC"))
ThisBuild / versionScheme        := Some("semver-spec")
ThisBuild / evictionErrorLevel   := Level.Warn
ThisBuild / scalaVersion         := "3.8.2"
ThisBuild / organization         := "io.github.edadma"
ThisBuild / organizationName     := "edadma"
ThisBuild / organizationHomepage := Some(url("https://github.com/edadma"))
ThisBuild / version              := "0.0.1"

ThisBuild / publishConfiguration := publishConfiguration.value.withOverwrite(true).withChecksums(Vector.empty)
ThisBuild / resolvers += Resolver.mavenLocal

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://github.com/edadma/trisc"),
    "scm:git@github.com:edadma/trisc.git",
  ),
)
ThisBuild / developers := List(
  Developer(
    id = "edadma",
    name = "Edward A. Maxedon, Sr.",
    email = "edadma@gmail.com",
    url = url("https://github.com/edadma"),
  ),
)

ThisBuild / homepage    := Some(url("https://github.com/edadma/trisc"))
ThisBuild / description := "TRISC - a 16-bit RISC CPU emulator and assembler"

publish / skip := true

lazy val trisc = project
  .in(file("."))
  .settings(
    name := "trisc",
    scalacOptions ++=
      Seq(
        "-deprecation",
        "-feature",
        "-unchecked",
        "-language:postfixOps",
        "-language:implicitConversions",
        "-language:existentials",
        "-language:dynamics",
      ),
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % "test",
    libraryDependencies ++= Seq(
      "org.scala-lang.modules" %% "scala-parser-combinators" % "2.4.0",
    ),
    libraryDependencies ++= Seq(
      "com.github.scopt" %% "scopt" % "4.1.0",
      "com.lihaoyi" %% "pprint" % "0.9.0",
    ),
    publishMavenStyle      := true,
    Test / publishArtifact := false,
  )
