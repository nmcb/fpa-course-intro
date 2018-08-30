package fpa
package effects
package test

import java.io.ByteArrayInputStream

import org.scalatest._

import scala.io.StdIn
import scala.util.Success

class EffectsSpec extends FlatSpec with Matchers {

  import io._
  import cats._
  import implicits._
  import effect._

  "IO" should "Q1 : encapsulate all possible side effects" in {
    """ main.unsafeRunSync """.stripMargin should compile
  }

  it should "Q2 : factor main out into a composition of pure expressions" in {
    """ val proofWriteUsagePure: IO[Unit]     = writeUsage """.stripMargin should compile
    """ val proofReadCelciusPure: IO[Double]  = readCelcius """.stripMargin should compile
    """ val proofWriteFarenheitPure: IO[Unit] = writeFarenheit(1.0) """.stripMargin should compile
  }

  it should "Q3 : Handle the div by zero case returning `Double.NaN` instead of throwing an exception " in {
    inverse1(0).unsafeRunSync() should be(Double.NaN)
  }

  it should "Q4 : Handle the div by zero case by transforming into a user specific exception " in {
    inverse2(0).attempt.unsafeRunSync() should be(Left(NoInverse("value has no inverse")))
  }

  it should "Q5 : On request evaluate it's IO expressions in the context of a legacy `Future`" in {
    import scala.concurrent.ExecutionContext.Implicits.global
    Console.withIn(new ByteArrayInputStream("FooBar".getBytes))  {
      client.name().onComplete {
        case Success(value) if value == "FooBar" => ()
        case _                                   => fail("Boom!")
      }
    }
  }
}
