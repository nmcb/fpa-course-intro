package playground

val someProductionList: List[Int] = List.range(1, 1000000)

def even[A](l: List[A]): Boolean = if (l.isEmpty) true  else odd(l.tail)
def odd[A](l: List[A]):  Boolean = if (l.isEmpty) false else even(l.tail)

def throwStackOverflowException(): Unit = even(someProductionList)

import cats.effect._
import unsafe.implicits.global

def lazyEven[A](l: List[A]): IO[Boolean] = if (l.isEmpty) IO(true)  else IO.defer(lazyOdd(l.tail))
def lazyOdd[A](l: List[A]):  IO[Boolean] = if (l.isEmpty) IO(false) else IO.defer(lazyEven(l.tail))


sealed trait Pure[A] {
  final def compute: A = this match {
    case Done(a) => a
    case Call(t) => t().compute
    case Cont(p,f) => p match {
      case Done(a) => f(a).compute
      case Call(t) => t().flatMap(f).compute
      case Cont(pp,ff) => pp.flatMap(a => ff(a).flatMap(f)).compute
    }
  }
  def flatMap[B](f: A => Pure[B]): Pure[B] = this match {
    case Done(a)       => Call(() => f(a))
    case p: Call[A]    => Cont(p, f)
    case c: Cont[a1,_] => Cont(c.p, (a: a1) => c.f(a).flatMap(f))
  }
  def map[B](f: A => B): Pure[B] =
    flatMap(a => Done(f(a)))
}
case class Done[A](a: A)                          extends Pure[A]
case class Call[A](t: () => Pure[A])              extends Pure[A]
case class Cont[A,B](p: Pure[A], f: A => Pure[B]) extends Pure[B]

def done[A](a: => A): Pure[A]       = Done(a)
def call[A](p: => Pure[A]): Pure[A] = Call(() => p)

def diyEven[A](l: List[A]): Pure[Boolean] = if (l.isEmpty) done(true)  else call(diyOdd(l.tail))
def diyOdd[A](l: List[A]):  Pure[Boolean] = if (l.isEmpty) done(false) else call(diyEven(l.tail))

object Play extends App:

  val trustTypelevel: Unit = lazyEven(someProductionList).unsafeRunSync()
  val trustYourself: Unit = diyEven(someProductionList).compute