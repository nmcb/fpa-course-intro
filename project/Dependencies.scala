import sbt._

object Dependencies {
  lazy val kindProjector = "org.typelevel"  %% "kind-projector" % "0.13.2" cross CrossVersion.full

  lazy val catsCore      = "org.typelevel"  %% "cats-core"      % "2.10.0"
  lazy val catsEffect    = "org.typelevel"  %% "cats-effect"    % "3.5.4"

  lazy val scalaTest     = "org.scalatest"  %% "scalatest"      % "3.2.18"
  lazy val scalaCheck    = "org.scalacheck" %% "scalacheck"     % "1.18.0"
}
