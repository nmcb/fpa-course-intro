import sbt._

object Dependencies {
  lazy val kindProjector = "org.typelevel"  %% "kind-projector" % "0.11.0" cross CrossVersion.full
  lazy val catsCore      = "org.typelevel"  %% "cats-core"      % "2.1.1"
  lazy val catsEffect    = "org.typelevel"  %% "cats-effect"    % "2.1.2"

  lazy val fastparse     = "com.lihaoyi"    %% "fastparse"      % "2.2.4"
  lazy val uPickle       = "com.lihaoyi"    %% "upickle"        % "1.0.0"

  lazy val scalaTest     = "org.scalatest"  %% "scalatest"      % "3.1.1"
  lazy val scalaCheck    = "org.scalacheck" %% "scalacheck"     % "1.14.0"
}
