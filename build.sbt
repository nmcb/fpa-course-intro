import Dependencies._

lazy val `fpa-course-intro` = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "fpa",
      scalaVersion := "2.12.6",
      version      := "0.1.0"
    )),
    name := "fpa-course-intro",
    libraryDependencies ++= Seq(
      scalaTest  % Test,
      scalaCheck % Test
    ))

scalacOptions := Seq(
  "-unchecked",
  "-deprecation",
  "-feature",
  "-language:higherKinds"
)