package fpa

object recursion extends App {

  def odd_[A](l: List[A]): Boolean =
    if (l.isEmpty) false else even_(l.tail)

  def even_[A](l: List[A]): Boolean =
    if (l.isEmpty) true else odd_(l.tail)

  println(even_((1 until 1000000).toList))

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

  def done[A](a: A): Pure[A] =
    Done(a)

  def call[A](p: => Pure[A]): Pure[A] =
    Call(() => p)

}