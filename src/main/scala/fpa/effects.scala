package fpa
package effects

import cats._
import implicits._
import effect._

import scala.io.StdIn

object intro extends App {

  /** Let's have a look at equational reasoning, given the pure function `twice` : */
  def twice[A : Semigroup](a1: A, a2: A): A = Semigroup[A].combine(a1, a2)

  /** And a lazy pure expression, in this example the calculation `1 + 2` : */
  lazy val x: Int = 1 + 2

  /** The following two programs are equivalent as expression names are equivalent to the expression value : */
  val pureProg1: Int = twice(x, x)
  val pureProg2: Int = twice(1 + 2, 1 + 2)

  /** But in the face of side-effects we loose our ability to reason about programs : */
  lazy val y: Int = {
    println("evaluating y = 1 + 2")
    1 + 2
  }

  /** As equational reasoning becomes dependent on the order and memoization strategy of expression evaluation : */
  val impureProg1: Int = twice(y, y)
  val impureProg2: Int = twice(
    { println("evaluating y = 1 + 2") ; 1 + 2},
    { println("evaluating y = 1 + 2") ; 1 + 2}
  )

  /** It is the lack of this reasonably required mental equational reasoning toolset that effect types try to combat */
  val z: IO[Int] = IO {
    println("evaluating z = 1 + 2")
    1 + 2
  }

  /** Unfortunately, this requires a new program structure ... as we now enforce pure expression passing */
  def twice[A : Semigroup](a1: IO[A], a2: IO[A]): IO[A] = for {
    x <- a1
    y <- a2
  } yield Semigroup[A].combine(x, y)

  val pureSideEffectingProg1: IO[Int] = twice(z, z)
  val pureSideEffectingProg2: IO[Int] = twice(
    IO {
      println("evaluating z = 1 + 2")
      1 + 2
    },
    IO {
      println("evaluating z = 1 + 2")
      1 + 2
    }
  )

  /** At the end of the world ... we can execute both programs : */

  pureSideEffectingProg1.unsafeRunSync()
  pureSideEffectingProg2.unsafeRunSync()

  /** And observe that the programs have equal output (both run the println effect twice) */

  /** SANITY HAS BEEN RESTORED */
}

object io extends App {

  /** Q1 : Refactor the imperative application below into an `IO` for comprehension. */
  def f2c(f: Double): Double = (f - 32) * 5.0 / 9.0

  val main: Unit = {
    print(s"=>> Enter a temperature in degrees °F : ")
    val fahrenheit = StdIn.readDouble
    println(s"=>> $fahrenheit °F === ${f2c(fahrenheit)} °C")
  }

  /** Q2 : Factor the sub expressions out in separate `IO` expressions */
  // def writeUsage
  // def readCelcius
  // def writeFarenheit(f: Double)

  /**
    * `MonadError` provides methods to handle exceptions in a monadic side channel :
    *
    * - attempt[A](fa: F[A]): F[Either[E, A]]
    * - raiseError[A](e: E): F[A]
    * - handleError[A](fa: F[A])(f: (E) ⇒ A): F[A]
    * - handleErrorWith[A](fa: F[A])(f: (E) ⇒ F[A]): F[A]
    * - adaptError[A](fa: F[A])(pf: PartialFunction[E, E]): F[A]
    *
    **/

  /** Q3: Handle the div by zero case by returning `Double.NaN`. */
  def inverse1(d: Double): IO[Double] = for {
    inverse <- (1 / d)
  } yield inverse

  /** Q4: Handle the div by zero case by transforming into a user specific exception. */
  case class NoInverse(msg: String = "value has no inverse") extends RuntimeException(msg)
  def inverse2(d: Double): IO[Double] = for {
    inverse <- (1 / d)
  } yield inverse
}