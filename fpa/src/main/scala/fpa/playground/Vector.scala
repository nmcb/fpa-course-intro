package fpa
package playground

import scala.annotation.tailrec

object Vector:

  // inductive definitions used to treat a type-parameter `N` as a natural number
  trait Z
  trait S[N]

  // A linked list of `A`s using phantom type `N` to encode the list's length
  sealed trait Vec[+A, +Nat]:

    import Vec.*

    def ::[B >: A, N >: Nat](b: B): Vec[B, S[N]] =
      cons(b, this)

    override def toString: String =
      mkString(this, "Vec(", ", ", ")")

  case class Empty[A]()                           extends Vec[A, Z]
  case class Cons[A, N](head: A, tail: Vec[A, N]) extends Vec[A, S[N]]


  object Vec:

    // constructors
    def empty[A]: Vec[A, Z] =
      Empty[A]()

    def cons[A, N](a: A, tail: Vec[A, N]): Vec[A, S[N]] =
      Cons(a, tail)

    // combinators
    def foldLeft[A, B, N](vec: Vec[A, N])(b: B)(f: (B, A) => B): B =
      @tailrec
      def loop(v: Vec[A, ?], acc: B = b): B =
        v match
          case Empty()    => acc
          case Cons(h, t) => loop(t, f(acc, h))
      loop(vec)

    def reverse[A, N](v: Vec[A, N]): Vec[A, N] =
      @tailrec
      def loop(v: Vec[A, ?], acc: Vec[A, ?] = empty[A]): Vec[A, N] =
        v match
          case Empty() => acc.asInstanceOf[Vec[A, N]]
          case Cons(h, t) => loop(t, cons(h, acc))
      loop(v)

    def map[A, B, N](fa: Vec[A, N])(f: A => B): Vec[B, N] =
      @tailrec
      def loop(fa: Vec[A, ?], f: A => B, acc: Vec[B, ?] = empty[B]): Vec[B, N] =
        fa match
          case Empty()    => reverse(acc).asInstanceOf[Vec[B, N]]
          case Cons(h, t) => loop(t, f, cons(f(h), acc))
      loop(fa, f)

    // utilities
    def mkString[A](vec: Vec[A, ?], pre: String, sep: String, post: String): String =
      @tailrec
      def loop(v: Vec[A, ?], acc: String = ""): String =
        v match
          case Empty()          => acc
          case Cons(h, Empty()) => acc + h.toString
          case Cons(h, t)       => loop(t, acc + h.toString + sep)
      pre + loop(vec) + post

  @main
  /** Run a couple of use-cases */
  def runVector(args: String*): Unit =
    import Vec.*
    val ints = 65 :: 66 :: 67 :: empty
    val strs = map(ints)(_.toChar)
    val sum  = foldLeft(ints)(0)(_+_)
    println("ints = " + ints.toString)
    println("strs = " + strs.toString)
    println("sum  = " + sum)
