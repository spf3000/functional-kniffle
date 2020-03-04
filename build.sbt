import Dependencies._


ThisBuild / version := "0.1.0-SNAPSHOT"
lazy val root = (project in file(".")).settings(
  inThisBuild(
    List(
      organization := "alandevlin",
      scalaVersion := "2.12.10",
      scalacOptions ++= Seq(
      "-Xfatal-warnings",
  "-deprecation",
  "-unchecked")
    )),
  name := "kniffle",
  libraryDependencies += "dev.zio"  %% "zio" % "1.0.0-RC18-1",
  libraryDependencies += "org.scalaz"  %% "scalaz-core" % "7.3.0-M31"
)
