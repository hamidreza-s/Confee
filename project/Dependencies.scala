import sbt._

object Dependencies {
  lazy val parserCombinators = "org.scala-lang.modules" %% "scala-parser-combinators" % "1.0.4"
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.0.5"
  lazy val circeCore = "io.circe" %% "circe-core" % "0.12.3"
  lazy val circeGeneric = "io.circe" %% "circe-generic" % "0.12.3"
  lazy val circeParser = "io.circe" %% "circe-parser" % "0.12.3"
}
