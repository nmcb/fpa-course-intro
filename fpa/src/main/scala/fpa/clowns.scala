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
  * we shall see how to turn a cata into a first-order tail-recursion whenever p is polynomial
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
import S2.*
case class P2[L[_, _], R[_, _], A, B](la: L[A, B], ra: R[A, B])        // product type aka tuple


/** and yes - the kit is bifunctorial */
trait Bifunctor[F[_,_]]:
  def bimap[A, B, C, D](f: A => C)(g: B => D)(fab: F[A, B]): F[C, D]

implicit def k2Functor[X]: Bifunctor[K2[X, *, *]] =
  new Bifunctor[K2[X, *, *]]:
    def bimap[A, B, C, D](f: A => C)(g: B => D)(fab: K2[X, A, B]): K2[X, C, D] = K2(fab.const)

implicit def fstFunctor: Bifunctor[Fst[*, *]] =
  new Bifunctor[Fst[*, *]]:
    def bimap[A, B, C, D](f: A => C)(g: B => D)(fab: Fst[A, B]): Fst[C, D] = Fst(f(fab.a))

implicit def sndFunctor: Bifunctor[Snd[*, *]] =
  new Bifunctor[Snd[*, *]]:
    def bimap[A, B, C, D](f: A => C)(g: B => D)(fab: Snd[A, B]): Snd[C, D] = Snd(g(fab.b))

implicit def s2Functor[L[_, _], R[_, _]](implicit lFunctor: Bifunctor[L], rFunctor: Bifunctor[R]): Bifunctor[S2[L, R, *, *]] =
  new Bifunctor[S2[L, R, *, *]]:
    def bimap[A, B, C, D](f: A => C)(g: B => D)(fab: S2[L, R, A, B]): S2[L, R, C, D] =
      fab match
        case L2(la) => L2(lFunctor.bimap(f)(g)(la))
        case R2(rb) => R2(rFunctor.bimap(f)(g)(rb))

implicit def p2Functor[L[_, _], R[_, _]](implicit lFunctor: Bifunctor[L], rFunctor: Bifunctor[R]): Bifunctor[P2[L, R, *, *]] =
  new Bifunctor[P2[L, R, *, *]]:
    def bimap[A, B, C, D](f: A => C)(g: B => D)(fab: P2[L, R, A, B]): P2[L, R, C, D] =
      P2(lFunctor.bimap(f)(g)(fab.la), rFunctor.bimap(f)(g)(fab.ra))


/** but.. but.. nothing is missing - we need non-constructable zero */
sealed trait Zero

def magic[A](z: Zero): A =
  sys.error("we never get this far")

/** still, nothing is functorial considering F[Zero] as a container with zero elements */
def inflate1[F[_], A](fz: F[Zero])(implicit zFunctor: Functor[F]): F[A] =
  zFunctor.fmap(magic)(fz)

def inflate2[F[_], A](fz: F[Zero])(implicit  zFunctor: Functor[F]): F[A] =
  fz.asInstanceOf[F[A]]

/** and nothing can be defined in terms of our functorial and bifuncorial kit as the constant zero */
type Zero1[A]    = K[Zero, A]
type Zero2[A, B] = K2[Zero, A, B]

/** with that we can dissect left clowns and right jokers */
case class Clown[F[_], C, J](fc: F[C])

implicit def clownBifunctor[F[_]](implicit functor: Functor[F]): Bifunctor[Clown[F, *, *]] =
  new Bifunctor[Clown[F, *, *]]:
    override def bimap[A, B, C, D](f: A => C)(g: B => D)(fab: Clown[F, A, B]): Clown[F, C, D] =
      Clown(functor.fmap(f)(fab.fc))

/** note that I1 ∼= Fst. */

case class Joker[F[_], C, J](fj: F[J])

implicit def jokerBifunctor[F[_]](implicit functor: Functor[F]): Bifunctor[Joker[F, *, *]] =
  new Bifunctor[Joker[F, *, *]]:
    override def bimap[A, B, C, D](f: A => C)(g: B => D)(fab: Joker[F, A, B]): Joker[F, C, D] =
      Joker(functor.fmap(g)(fab.fj))


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
