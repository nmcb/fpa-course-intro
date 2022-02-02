import Dependencies._

lazy val mainDependencies = Seq(
  catsCore,
  catsEffect,
  fastparse,
  uPickle
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
      scalaVersion := "2.13.8",
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
  "-language:implicitConversions"
)
