package fpa
package playground

import org.scalatest._


class IOTest extends FlatSpec with Matchers {

  "IO" should "encapsulate programs" in {
    val prog = IO.delay(1)
    prog.unsafePerformIO should be(1)
  }

  it should "be stack-safe" in {

    def even(l: List[Int]): IO[Boolean] =
      if (l.isEmpty) IO.delay(true) else IO.suspend(odd(l.tail))

    def odd(l: List[Int]): IO[Boolean] =
      if (l.isEmpty) IO.delay(false) else IO.suspend(even(l.tail))

    even((1 to 1000000).toList).unsafePerformIO should be (true)

    /** Takes 3 min to finish, enable to validate continuation testing of stack safety */
    def ackermann(m: Int, n: Int): IO[Int] = (m, n) match {
      case (0, n) => IO.delay(n + 1)
      case (m, 0) => IO.suspend(ackermann(m - 1, 1))
      case _      => for {
        inner <- IO.suspend(ackermann(m, n - 1))
        outer <- IO.suspend(ackermann(m - 1, inner))
      } yield outer
    }

    ackermann(4, 1).unsafePerformIO should be (65533)
  }
}
