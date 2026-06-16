import Dependencies.*

ThisBuild / organization := "fpa"
ThisBuild / scalaVersion := "3.8.4"
ThisBuild / version      := "0.1.0"
ThisBuild / scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-feature",
  "-language:higherKinds"
)

lazy val cats: Seq[ModuleID] =
  Seq(catsCore, catsEffect)

lazy val test: Seq[ModuleID] =
  Seq(scalaTest, scalaCheck).map(_ % Test)

lazy val course: Project =
  (project in file("."))
    .aggregate(fpa, fps)

lazy val fpa: Project =
  (project in file("fpa"))
    .settings(
      name                 := "fpa-course-intro",
      libraryDependencies ++= cats ++ test,
      scalacOptions       ++= Seq("-Xkind-projector:underscores"),
      run / javaOptions    += "-Xss1m"
    )

lazy val fps: Project =
  (project in file("fps"))
    .settings(
      name                 := "fps-course",
      libraryDependencies ++= cats ++ test
    )
