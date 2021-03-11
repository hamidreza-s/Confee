import Dependencies._

ThisBuild / scalaVersion     := "2.13.4"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
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
      scalaTest         % Test
    )
  )

lazy val `confee-cli` = project
  .in(file("confee-cli"))
  .settings(
    name := "confee-cli",
    libraryDependencies ++=Seq(
      "com.github.scopt" %%% "scopt" % "4.0.1" % Compile
    )
  )
  .enablePlugins(ScalaNativePlugin)
  .dependsOn(`confee-compiler`)
