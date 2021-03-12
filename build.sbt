import Dependencies._

ThisBuild / scalaVersion := "2.13.4"
ThisBuild / version := "0.0.1-SNAPSHOT"
ThisBuild / organization := "com.spotify"
ThisBuild / organizationName := "spotify"

lazy val root = project
  .in(file("."))
  .settings(name := "confee-parent")
  .aggregate(`confee-compiler`, `confee-cli`)

lazy val `confee-compiler` = project
  .in(file("confee-compiler"))
  .settings(
    name := "confee-compiler",
    libraryDependencies ++= Seq(
      parserCombinators % Compile,
      circeCore         % Compile,
      circeGeneric      % Compile,
      circeParser       % Compile,
      circeYaml         % Compile,
      scopt             % Compile,
      scalaTest         % Test,
      scalactic         % Test
    )
  )

lazy val `confee-cli` = project
  .in(file("confee-cli"))
  .settings(
    name := "confee-cli",
    libraryDependencies ++= Seq(
      scopt % Compile
    )
  )
  .dependsOn(`confee-compiler`)
