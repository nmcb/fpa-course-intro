package fpa
package clowns

enum Expr0:
  case Val(i: Int)
  case Add(e1: Expr0, e2: Expr0)

import Expr0.*


/** non-tail recursive evaluator */

def eval1(e: Expr0): Int =
  e match
    case Val(i)      => i
    case Add(e1, e2) => eval1(e1) + eval1(e2)


/** tail recursive evaluator employing a stack */

type Stack = List[Either[Expr0, Int]]

def eval2(e: Expr0): Int =

  def load(e: Expr0)(stk: Stack): Int =
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
import S.*
case class P[L[_], R[_], A](la: L[A], ra: R[A])       // product type aka tuple



/** eg. unit generically defined as a constant */

type One[A] = K[Unit,A]


/** or eg. the option type generically defined as a sum type */

type Maybe[A] = S[One,I,A]

def nothing[A]: Maybe[A] =
  L(K(()))

def just[A](a: A): Maybe[A] =
  R(I(a))


/** the kit is functorial */

trait Functor[F[_]]:
  def fmap[A,B](f: A => B)(fa: F[A]): F[B]

implicit def kFunctor[X]: Functor[K[X, *]] =
  new Functor[K[X, *]]:
    def fmap[A, B](f: A => B)(fa: K[X, A]): K[X, B] = K(fa.a)

implicit def iFunctor: Functor[I[_]] =
  new Functor[I[_]]:
    def fmap[A, B](f: A => B)(fa: I[A]): I[B] = I(f(fa.a))

implicit def sFunctor[L[_], R[_]](implicit lFunctor: Functor[L], rFunctor: Functor[R]): Functor[S[L, R, _]] =
  new Functor[S[L, R, _]]:
    def fmap[A, B](f: A => B)(fa: S[L, R, A]): S[L, R, B] =
      fa match
        case L(la) => L(lFunctor.fmap(f)(la))
        case R(ra) => R(rFunctor.fmap(f)(ra))

implicit def pFunctor[L[_], R[_]](implicit lFunctor: Functor[L], rFunctor: Functor[R]): Functor[P[L, R, _]] =
  new Functor[P[L, R, _]]:
    def fmap[A, B](f: A => B)(fa: P[L, R, A]): P[L, R, B] =
      P(lFunctor.fmap(f)(fa.la), rFunctor.fmap(f)(fa.ra))


/** the expr branching structure is readily described by a polynomial */
type ExprP[A] = S[K[Int, *], P[I, I, *], A]

def valP[A](i: Int): ExprP[A] =
  L(K(i))

object ValP:
  def unapply(ep: ExprP[Int]): Option[Int] =
    ep match
      case L(K(i)) => Some(i)
      case _       => None

def addP[A](e1: A, e2: A): ExprP[A] =
  R(P(I(e1), I(e2)))

object AddP:
  def unapply(ep: ExprP[Int]): Option[(Int, Int)] =
    ep match
      case R(P(I(e1), I(e2))) => Some(e1, e2)
      case _                  => None


/** we would like now to establish the isomorphism: Expr ∼= ExprP Expr */
/** which we do via a type level fix point to tie the knot inductively */
case class Mu[F[_]](in: F[Mu[F]])

type Expr = Mu[ExprP]

def valM(i: Int): Expr =
  Mu(valP(i))

def addM[A](e1: Expr, e2: Expr): Expr =
  Mu(addP(e1, e2))


/** this lets us define a fold like recursion operator as a catamorphism - see doc/img/cata.png */
def cata[P[_], A](phi: P[A] => A)(p: Mu[P])(implicit pFunctor: Functor[P]): A =
  phi(pFunctor.fmap(cata(phi))(p.in))

/** with a subsequent evaluator in terms of that catamorphism */
def  eval3(e: Expr): Int =
  def phi(e: ExprP[Int]): Int =
    e match
      case ValP(i)      => i
      case AddP(e1, e2) => e1 + e2
      case _            => sys.error("boom")
  cata(phi)(e)


/**
  * we shall see how to turn a cata into a first- order tail-recursion whenever p is polynomial
  * we shall do this by dissecting p, with ‘clown’ elements left and ‘joker’s on the right
  * to this end, we need polynomial bifunctors, which are just functors, but in two directions
  * let's construct a data component kit for that with two generic type parameters `x` and `y`
  * generic data component kit
  **/
case class K2[X, A, B](const: X)                                       // constant
case class Fst[A, B](a: A)                                             // first identity
case class Snd[A, B](b: B)                                             // second identity
enum S2[L[_, _], R[_, _], A, B]:                                       // sum type aka either
  case L2[L[_, _], R[_, _], A, B](la: L[A, B]) extends S2[L, R, A, B]
  case R2[L[_, _], R[_, _], A, B](ra: R[A, B]) extends S2[L, R, A, B]
import S.*
case class P2[L[_, _], R[_, _], A, B](la: L[A, B], ra: R[A, B])        // product type aka tuple


/** and yes - the kit is bifunctorial */
trait Bifunctor[F[_,_]]:
  def bimap[A, B, C, D](f: A => B)(g: C => D)(fab: F[A, B]): F[C, D]

implicit def k2Functor[X]: Bifunctor[K2[X, *, *]] =
  new Bifunctor[K2[X, *, *]]:
    def bimap[A, B, C, D](f: A => B)(g: C => D)(fab: K2[X, A, B]): K2[X, C, D] = K2(fab.const)

object Main extends App:

  val e0: Expr0 = Add(Add(Val(1), Val(2)), Val(3))

  val a1 = eval1(e0)
  val a2 = eval2(e0)
  println(s"eval1($e)=$a1")
  println(s"eval2($e)=$a2")
  assert(a1 == 6)
  assert(a2 == 6)

  val m1 = implicitly[Functor[Maybe]].fmap[Char,Boolean](_.isDigit)(just('1'))
  val m2 = implicitly[Functor[Maybe]].fmap[Char,Boolean](_.isDigit)(nothing[Char])
  println(s"implicitly[Functor[Maybe]].fmap[Char,Boolean](_.isDigit)(just('1'))=$m1")
  println(s"implicitly[Functor[Maybe]].fmap[Char,Boolean](_.isDigit)(nothing[Char])=$m2")
  assert(m1 == R(I(true)))
  assert(m2 == L(K(())))

  val e: Expr = addM(addM(valM(1), valM(2)), valM(3))

  val a3 = eval3(e)
  println(s"eval3($e)=$a3")
  assert(a2 == 6)
