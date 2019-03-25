package fpa
package playground

import org.scalatest._


class IOTest extends FlatSpec with Matchers {

  "IO" should "encapsulate programs" in {
    val prog = IO.delay(1)
    prog.unsafePerformIO should be(1)
  }

  // Takes 3 min to finish, enable to validate stack safety
  ignore should "be stack-safe" in {

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
