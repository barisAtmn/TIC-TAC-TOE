ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

libraryDependencies += "dev.zio" %% "zio" % "2.0.0"
libraryDependencies += "dev.zio" %% "zio-macros" % "2.0.0"
libraryDependencies += "dev.zio" %% "zio-test" % "2.0.0"
libraryDependencies += "dev.zio" %% "zio-test-sbt" % "2.0.0"


lazy val root = (project in file("."))
  .settings(
    name := "tic-tac-toe"
  )
