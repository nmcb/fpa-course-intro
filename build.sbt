import Dependencies._

scalaVersion := "2.12.7"

lazy val mainDependencies = Seq(
  catsCore,
  catsEffect,
  fastparse
)

lazy val testDependencies = Seq(
  scalaTest,
  scalaCheck
).map(_ % Test)

lazy val `fpa-course-intro` = (project in file(".")).
  settings(
    name := "fpa-course-intro",
    inThisBuild(List(
      organization := "fpa",
      scalaVersion := "2.12.6",
      version      := "0.1.0"
    )),
    addCompilerPlugin(kindProjector),
    libraryDependencies ++= mainDependencies ++ testDependencies
  )

scalacOptions := Seq(
  "-unchecked",
  "-deprecation",
  "-feature",
  "-language:higherKinds",
  "-language:implicitConversions",
  "-Ypartial-unification",
  "-Ywarn-unused-import"
)