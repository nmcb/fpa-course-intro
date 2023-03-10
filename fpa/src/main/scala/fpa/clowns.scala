package fpa
package clowns

enum Expr:
  case Val(i: Int)
  case Add(e1: Expr, e2: Expr)

import Expr.*


/** non-tail recursive evaluator */

def eval1(e: Expr): Int =
  e match
    case Val(i)      => i
    case Add(e1, e2) => eval1(e1) + eval1(e2)



/** tail recursive evaluator employing a stack */

type Stack = List[Either[Expr, Int]]

def eval2(e: Expr): Int =

  def load(e: Expr)(stk: Stack): Int =
    e match
      case Val(i)      => unload(i)(stk)
      case Add(e1, e2) => load(e1)(Left(e2) :: stk)

  def unload(v1: Int)(stk: Stack): Int =
    stk match
      case Nil               => v1
      case Left(e2) :: rest  => load(e2)(Right(v1) :: rest)
      case Right(v2) :: rest => unload(v1 + v2)(rest)

  load(e)(List.empty)


/** generic data component kit */

case class K[A,B](a: A)                               // constant
case class I[A](a: A)                                 // identity
enum S[L[_], R[_], A]:                                // sum type aka either
  case L[L[_], R[_], A](la: L[A]) extends S[L, R, A]
  case R[L[_], R[_], B](ra: R[B]) extends S[L, R, B]
case class P[L[_], R[_], A](la: L[A], ra: R[A])    // product type aka tuple

import S.*


/** eg. unit generically defined as a constant */

type One[A] = K[Unit,A]


/** or eg. the option type generically defined as a sum type */

type Maybe[A] = S[One,I,A]

def nothing: Maybe[Unit] =
  L(K(()))

def just[A](a: A): Maybe[A] =
  R(I(a))


trait Functor[F[_]]:
  def fmap[A,B](fa: F[A])(f: A => B): F[B]

implicit def kFunctor[X]: Functor[K[X, *]] =
  new Functor[K[X, *]]:
    def fmap[A, B](fa: K[X, A])(f: A => B): K[X, B] = K(fa.a)

implicit def iFunctor: Functor[I[_]] =
  new Functor[I[_]]:
    def fmap[A, B](fa: I[A])(f: A => B): I[B] = I(f(fa.a))

implicit def sFunctor[L[_], R[_]](implicit lFunctor: Functor[L], rFunctor: Functor[R]): Functor[S[L, R, _]] =
  new Functor[S[L, R, _]]:
    def fmap[A, B](fa: S[L, R, A])(f: A => B): S[L, R, B] =
      fa match
        case L(la) => L(lFunctor.fmap(la)(f))
        case R(ra) => R(rFunctor.fmap(ra)(f))

implicit def pFunctor[L[_], R[_]](implicit lFunctor: Functor[L], rFunctor: Functor[R]): Functor[P[L, R, _]] =
  new Functor[P[L, R, _]]:
    def fmap[A, B](fa: P[L, R, A])(f: A => B): P[L, R, B] =
      P(lFunctor.fmap(fa.la)(f), rFunctor.fmap(fa.ra)(f))


object MainClown extends App:

  val e = Add(Add(Val(1), Val(2)), Val(3))

  println(eval1(e))
  println(eval2(e))
  
  println(implicitly[Functor[Maybe]].fmap(just('1'))(_.isDigit))

