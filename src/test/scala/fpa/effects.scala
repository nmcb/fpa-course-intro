package fpa
package effects
package test

import org.scalatest._

class EffectsSpec extends FlatSpec with Matchers {

  import io._
  import cats._
  import implicits._
  import effect._

  "IO" should "Q1 : encapsulate all possible side effects" in {
    """ main.unsafeRunSync """.stripMargin should compile
  }

  "IO" should "Q2 : factor main out into a composition of pure expressions" in {
    """ val proofWriteUsagePure: IO[Unit]     = writeUsage """.stripMargin should compile
    """ val proofReadCelciusPure: IO[Double]  = readCelcius """.stripMargin should compile
    """ val proofWriteFarenheitPure: IO[Unit] = writeFarenheit(1.0) """.stripMargin should compile
  }

  "IO" should "Q3 : Handle the div by zero case returning `Double.NaN` instead of throwing an exception " in {
    inverse1(0).unsafeRunSync() should be(Double.NaN)
  }

  "IO" should "Q4 : Handle the div by zero case by transforming into a user specific exception " in {
    inverse2(0).attempt.unsafeRunSync() should be(Left(NoInverse("value has no inverse")))
  }
}
