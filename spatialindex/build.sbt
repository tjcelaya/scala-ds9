import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.11.1",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "Hello",
    libraryDependencies ++= Seq(
      scalaTest % Test,
      "com.github.jlmauduy" %% "ascii-graphs" % "0.0.7"
    )
  )
