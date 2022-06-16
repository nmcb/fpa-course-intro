package fpa
package playground

/** A synchronous poor man implementation of IO */
sealed trait Pure[A] {
  import Pure._

  @scala.annotation.tailrec
  final def compute: A = this match {
    case Done(a)    => a
    case Call(t)    => t().compute
    case Cont(p, f) => p match {
      case Done(a)      => f(a).compute
      case Call(t)      => t().flatMap(f).compute
      case Cont(pp, ff) => pp.flatMap(a => ff(a).flatMap(f)).compute
    }
  }

  def flatMap[B](f: A => Pure[B]): Pure[B] =
    this match {
      case Done(a)        => f(a)
      case c: Call[A]     => Cont(c, f)
      case c: Cont[a1,b1] => Cont(c.p, (a: a1) => c.f(a).flatMap(f))
    }

  def map[B](f: A => B): Pure[B] =
    flatMap(a => Done(f(a)))
}

object Pure {
  def call[A](p: => Pure[A]): Pure[A] = Call(() => p)
  def pure[A](a: => A): Pure[A]       = Call(() => Done(a))

  final case class Done[A](a: A)                            extends Pure[A]
  final case class Call[A](t: () => Pure[A])                extends Pure[A]
  final case class Cont[A, B](p: Pure[A], f: A => Pure[B])  extends Pure[B]
}

object MainPure extends App {
  import Pure._
  def ack(m: Int, n: Int): Pure[Int] = (m,n) match {
    case (0,_) => pure(n + 1)
    case (_,0) => call(ack(m - 1, 1))
    case (_,_) => call(for {
                    inner <- ack(m, n - 1)
                    outer <- ack(m - 1, inner)
                  } yield outer)
  }

  val m = 3
  val n = 12
  println(s"ack($m,$n) = ${ack(m,n).compute}")
}