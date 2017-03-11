import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "co.tjcelaya.sigfig_test.spatialindex",
      scalaVersion := "2.11.8",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "KdTree",
    libraryDependencies += scalaTest % Test
  )