package fpa
package playground

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers


class IOTest extends AnyFlatSpec with Matchers {

  "IO" should "encapsulate programs" in {
    val prog = Pure.pure(1)
    prog.compute should be(1)
  }

  it should "be stack-safe" in {

    def even(l: List[Int]): Pure[Boolean] =
      if (l.isEmpty) Pure.pure(true) else Pure.call(odd(l.tail))

    def odd(l: List[Int]): Pure[Boolean] =
      if (l.isEmpty) Pure.pure(false) else Pure.call(even(l.tail))

    even((1 to 1000000).toList).compute should be (true)
    even((1 to 1000001).toList).compute should be (false)
    
    odd((1 to 1000000).toList).compute should be (false)
    odd((1 to 1000001).toList).compute should be (true)

    /** ackermann function enables to validate continuation testing of stack safety */
    def ackermann(m: Int, n: Int): Pure[Int] =
      (m, n) match {
        case (0, n) => Pure.pure(n + 1)
        case (m, 0) => Pure.call(ackermann(m - 1, 1))
        case _      => for {
          inner <- Pure.call(ackermann(m, n - 1))
          outer <- Pure.call(ackermann(m - 1, inner))
        } yield outer
      }

    ackermann(3, 3).compute should be (61)

    /* takes approx 2 minutes to validate */
    // ackermann(4, 1).compute should be (65533)
  }
}
