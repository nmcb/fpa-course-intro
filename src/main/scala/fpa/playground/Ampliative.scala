package fpa
package playground

trait Ampliative[A] {
  def amplify(a: A): A
}

object Ampliative extends App {

  implicit def intAmpliative = new Ampliative[Int] {
    override def amplify(a: Int): Int = a + 1
  }

  import Nat.peano._
  val n = succ(succ(zero))
  println(n)
}


case class Nat[A](underlying: A)

object Nat {

  object peano {
    def zero: Nat[Int] =
      Nat(0)

    def succ(n: Nat[Int]) = {
      val successor = implicitly[Ampliative[Int]].amplify(n.underlying)
      Nat(successor)
    }
  }

}