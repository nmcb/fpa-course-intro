import sbt._

object Dependencies {
  lazy val kindProjector = "org.typelevel"  %% "kind-projector" % "0.13.2" cross CrossVersion.full

  lazy val catsCore      = "org.typelevel"  %% "cats-core"      % "2.7.0"
  lazy val catsEffect    = "org.typelevel"  %% "cats-effect"    % "3.3.10"

  lazy val scalaTest     = "org.scalatest"  %% "scalatest"      % "3.2.11"
  lazy val scalaCheck    = "org.scalacheck" %% "scalacheck"     % "1.15.4"
}
