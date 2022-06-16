package fpa
package demo

object lib {
  def A(m: Int, n: Int): Pure[Int] =
    (m , n) match {
      case (0, _) => done(n + 1)
      case (_, 0) => call(A(m - 1, 1))
      case (_, _) => for {
          inner <- call(A(m, n - 1))
          outer <- call(A(m - 1, inner))
      } yield outer
    }

  sealed trait Pure[A] {
    @scala.annotation.tailrec final def compute: A =
      this match {
        case Done(a) => a
        case Call(t) => t().compute
        case Cont(pa, f) => pa match {
          case Done(a) => f(a).compute
          case Call(t) => t().flatMap(a => f(a)).compute
          case Cont(ppa, ff) => ppa.flatMap(a => ff(a).flatMap(f)).compute
        }
      }


    def flatMap[B](f: A => Pure[B]): Pure[B] =
      this match {
        case Done(a)         => Call(() => f(a))
        case p : Call[A]     => Cont(p, f)
        case c : Cont[a1,b1] => Cont(c.pa, (a: a1) => c.f(a).flatMap(f))
      }

    def map[B](f: A => B): Pure[B] =
      flatMap(a => Done(f(a)))
  }
  case class Done[A](a: A) extends Pure[A]
  case class Call[A](f: () => Pure[A]) extends Pure[A]
  case class Cont[A,B](pa: Pure[A], f: A => Pure[B]) extends Pure[B]

  def done[A](a: A): Pure[A]    = Done(a)
  def call[A](pa: => Pure[A]): Pure[A] = Call(() => pa)
}

object Main extends App {

  import lib._

  println(s"A(4,1) = ${A(4,1).compute}")
}