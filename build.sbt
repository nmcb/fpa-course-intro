import Dependencies._

ThisBuild / organization := "fpa"
ThisBuild / version      := "0.1.0"

lazy val mainDependencies = Seq(
  catsCore,
  catsEffect
)

lazy val testDependencies = Seq(
  scalaTest,
  scalaCheck
).map(_ % Test)

lazy val root = (project in file("."))
  .aggregate(fpa, fps)

lazy val fpa = (project in file("fpa"))
  .settings(
    scalaVersion := "2.13.8",
    name := "fpa-course-intro",
    libraryDependencies ++= mainDependencies ++ testDependencies,
    addCompilerPlugin(kindProjector)
  )

lazy val fps = (project in file("fps")).
  settings(
    scalaVersion := "3.1.1",
    name := "fps-course",
    libraryDependencies ++= mainDependencies ++ testDependencies
  )

scalacOptions ++= Seq(
  "-unchecked",
  "-deprecation",
  "-feature",
  "-language:higherKinds"
)
