//object vector extends App {
//
//  // inductive definitions used to treat a type-parameter `N` as a natural number
//  trait Z
//  trait S[N]
//
//  // A linked list of `A`s using phantom type `N` to encode the list's length
//  sealed trait Vec[+A, +Nat] {
//
//    def ::[B >: A, N >: Nat](b: B): Vec[B, S[N]] =
//      cons(b, this)
//
//    override def toString: String =
//      mkString("Vec(", ", ", ")")(this)
//
//  }
//  case class Empty[A]()                           extends Vec[A, Z]
//  case class Cons[A, N](head: A, tail: Vec[A, N]) extends Vec[A, S[N]]
//
//
//
//  // constructors
//  def empty[A]: Vec[A, Z] =
//    new Empty[A]()
//
//  def cons[A, N](a: A, tail: Vec[A, N]): Vec[A, S[N]] =
//    new Cons(a, tail)
//
//  // combinators
//  def foldLeft[A, B](vec: Vec[A, _])(b: B)(f: (B, A) => B): B = {
//    @scala.annotation.tailrec def loop(v: Vec[A, _], acc: B = b): B = v match {
//      case Empty()    => acc
//      case Cons(h, t) => loop(t, f(acc, h))
//    }
//    loop(vec)
//  }
//
//  def reverse[A, N](vec: Vec[A, N]): Vec[A, N] =
//    foldLeft(vec)(empty[A])((v, a) => cons(a, v))
//
//  def map[A, B, N](fa: Vec[A, N])(f: A => B): Vec[B, N] = {
//    def loop(fa: Vec[A, _], f: A => B, acc: Vec[B, _] = empty[B]): Vec[B, N] =
//      fa match {
//        case Empty()    => acc.asInstanceOf[Vec[B, N]] // shut-up i know what i'm doing
//        case Cons(h, t) => loop(t, f, cons(f(h), acc))
//      }
//    loop(fa, f)
//  }
//
//  // utilities
//  def mkString[A](pre: String, sep: String, post: String)(vec: Vec[A, _]): String = {
//    @scala.annotation.tailrec def loop(v: Vec[A, _], acc: String = ""): String = v match {
//      case Empty()          => acc
//      case Cons(h, Empty()) => acc + h.toString
//      case Cons(h, t)       => loop(t, acc + h.toString + sep)
//    }
//    pre + loop(vec) + post
//  }
//
//  // a couple of use-cases
//  val ints = 1 :: 2 :: 3 :: empty
//  val strs = map(ints)(_.toString)
//  val sum  = foldLeft(ints)(0)(_+_)
//  println("ints = " + ints.toString)
//  println("strs = " + strs.toString)
//  println("sum  = " + sum)
//}
