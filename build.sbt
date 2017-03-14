import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "co.tjcelaya.ds9",
      scalaVersion := "2.11.8",
      version := "0.1.0-SNAPSHOT",
      scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")
    )),
    name := "DS9",
    libraryDependencies += scalaTest % Test
  )
