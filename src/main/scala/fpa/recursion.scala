object recursion extends App {

  import util._

  def odd[A](l: List[A]): Pure[Boolean]  = if (l.isEmpty) done(false) else call(even(l.tail))
  def even[A](l: List[A]): Pure[Boolean] = if (l.isEmpty) done(true) else call(odd(l.tail))

  println(even((1 until 1000000).toList))
}

object util {

  sealed trait Pure[A] {
    @scala.annotation.tailrec final def compute: A = this match {
      case Done(a) => a
      case Call(t) => t().compute
    }
  }
  case class Done[A](a: A) extends Pure[A]
  case class Call[A](t: () => Pure[A]) extends Pure[A]

  def done[A](a: A): Pure[A] = Done(a)
  def call[A](p: => Pure[A]): Pure[A] = Call(() => p)

}