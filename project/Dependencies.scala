import sbt._

object Dependencies {
  lazy val scalaTest     = "org.scalatest"  %% "scalatest"      % "3.0.5"
  lazy val scalaCheck    = "org.scalacheck" %% "scalacheck"     % "1.14.0"
  lazy val catsCore      = "org.typelevel"  %% "cats-core"      % "1.2.0"
  lazy val kindProjector = "org.spire-math" %% "kind-projector" % "0.9.7"
}
