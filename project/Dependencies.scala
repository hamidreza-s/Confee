import sbt._

object Dependencies {
  lazy val parserCombinators = "org.scala-lang.modules" %% "scala-parser-combinators" % "1.1.2"
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.2.5"
  lazy val scalaMock = "org.scalamock" %% "scalamock" % "5.1.0"
  lazy val scalactic = "org.scalactic" %% "scalactic" % "3.2.5"
  lazy val circeCore = "io.circe" %% "circe-core" % "0.12.3"
  lazy val circeGeneric = "io.circe" %% "circe-generic" % "0.12.3"
  lazy val circeParser = "io.circe" %% "circe-parser" % "0.12.3"
  lazy val circeYaml = "io.circe" %% "circe-yaml" % "0.12.0"
  lazy val scopt = "com.github.scopt" %% "scopt" % "4.0.1"
}
