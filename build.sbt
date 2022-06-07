import Dependencies._

ThisBuild / organization := "fpa"
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

lazy val root: Project =
  (project in file("."))
    .aggregate(fpa, fps)

lazy val fpa: Project =
  (project in file("fpa"))
    .settings(
      scalaVersion         := "2.13.8",
      name                 := "fpa-course-intro",
      run / javaOptions    += "-Xss1m",
      libraryDependencies ++= cats ++ test,
      addCompilerPlugin(kindProjector)
    )

lazy val fps: Project =
  (project in file("fps"))
    .settings(
      scalaVersion         := "3.1.2",
      name                 := "fps-course",
      libraryDependencies ++= cats ++ test
    )
