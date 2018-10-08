package fpa

object fixpoint {

  /*** Fixpoints at Value Level ***/

  /**
    *  Let us try to factor out the recursive part of a function.
    */
  def facRec: Int => Int =
    (n: Int) =>
      if (n == 0) 1 else n * facRec(n - 1)

  /**
    *  Replace the recursive call with a higher order function parameter.
    */
  def facLike: (Int => Int) => (Int => Int) =
    (f: Int => Int) =>
      (n: Int) =>
        if (n == 0) 1 else n * f(n - 1)

  /**
    *  For the moment, assume a library factorial function exists.
    */
  def facLib: Int => Int =
    ???

  /**
    *  Then it's easy to see that passing facLib as f is also factorial.
    */
  def alsoFac1: Int => Int =
    (n: Int) =>
      facLike(facLib)(n)           // Proof by substitution.

  /**
    *  Thus, could we define alsoFac as a recursive curried parameter?
    */
  def alsoFac2: Int => Int =
    (n: Int) =>
      facLike(alsoFac2)(n)		     // Yes! (But we need a lambda).

  /**
    *  Let's define a utility function.
    */
  def id[A]: A => A =
    (a: A) => a                    // Identity.

  /**
    *  Which makes it possible to calculate factorial for n <= 0
    */
  def facUpTo0: Int => Int =
    facLike(id)                    // But without a recursive call!

  /**
    *  Therefor we could "stack" the recursion explicitly for n <= 1.
    */
  def facUpTo1: Int => Int =
    facLike(facUpTo0)		 			     // Note, still no recursive call!

  /**
    *  And for n <= 2 ... etc., going up towards infinity.
    */
  def facUpTo2: Int => Int =
    facLike(facUpTo1)

  /**
    *  We could now "conceptually" define the fixpoint of facLike as:
    */
  def factorialUpToInfinity: Int => Int =
    facLike(factorialUpToInfinity) // Which never terminates ...

  /**
    *  Interlude: A fixed point of a function is the return value of
    *  that function after repeated evaluation.  E.g. the notion of
    *  a fixpoint should be familiar to anyone who has amused
    *  themselves playing with a pocket calculator.  You start with 0
    *  radians and hit the cos (cosine) key repeatedly.  What you find
    *  is that the answer rapidly converges to a number which is
    *  approximately 0.739085;  hitting the cos key again doesn't
    *  change anything because `cos(0.739085) = 0.739085`.  We say
    *  that the number 0.739085 is a fixpoint of the cosine function.
    */


  /**
    *  Thus: Let's derive the (untyped) Y-Combinator for this:
    *
    *  (Y f) == fixpoint-of-f                       --by definition
    *
    *  (f fixpoint-of-f) == fixpoint-of-f           --by definition
    *
    *  (Y f) == fixpoint-of-f == (f fixpoint-of-f)  --by equality
    *
    *  (Y f) == (f (Y f))                           --by substitution
    *
    *  Voila!
    */


  /**
    *  Typed, we need Y to accept a function:
    *
    *  a)  That takes a function of A to A to a function of A to A.
    *
    *  b)  So we can return a function from A to A.
    *
    *  Note that the input's function type is, itself, the return type.
    */
  def Y[A]: ((A => A) => (A => A)) => (A => A) =
    (f: (A => A) => (A => A)) =>
      (a: A) =>
        f(Y(f))(a)

  /**
    *  So we compose the fixpoint of facLike, thus defining factorial!
    */
  def fixFac: Int => Int =
    Y(facLike)



  /*** Fixpoints at Type Level ***/

  import scala.language.higherKinds

  /**
    *  Let's start with defining a _value_ level fixpoint for some F.
    */
  case class Fix[F[_]](f: F[Fix[F]])

  /**
    *  And try to use that implementing a generic (non-recursive) ADT.
    */
  trait ListLike[+A, +S]
  case class ConsLike[A, +S](a: A, as: S) extends ListLike[A, S]
  trait NilLike extends ListLike[Nothing, Nothing]
  object NilLike extends NilLike

  /**
    *  This means we need to be able to fix that type's point and
    *  a means to encode the two constructor functions that allow us to
    *  construct a List in terms of a fixpoint calculation.
    */
  type List[A] =
    Fix[ListLike[A, ?]]

  def nil[A]: List[A] =
    Fix[ListLike[A, ?]](NilLike)

  def cons[A](x: A, xs: List[A]): List[A] =
    Fix[ListLike[A, ?]](ConsLike(x, xs))

  /**
    *  With that we are already able to build lists at value level, e.g.
    */
  val xs = cons(1, cons(2, cons(3, nil)))


  /**
    *  Now onto fixpoint calculations at type level.
    */
  trait Inductive
  trait INil extends Inductive
  case class IFix[F[_], R <: Inductive](f: F[R]) extends Inductive

  /**
    *  Note 1)  Since R has kind, we have recursion at the type level.  So
    *           contrarily to Fix, not every element in the recursion have
    *           the same type.
    *
    *  Note 2)  At some point we'll need to stop the recursion. The INil
    *           will have no inhabitant, and just serves that purpose.
    */

  /**
    *  Now how does this help us implementing HList?  Well, here it becomes
    *  really cool.  You see, the only difference between a List and a HList
    *  is the recursion scheme.  A HList is recursive at both the type and
    *  value level at the same time, while a List is only recursive at the
    *  value level.  Apart from that, they are the same.  Therefore, we can
    *  reuse the previously defined Nil and Cons, and we just have to provide
    *  new constructors.
    */
  type HNil =
    IFix[ListLike[NilLike, ?], INil]

  type ::[A, AS <: Inductive] =
    IFix[ListLike[A, ?], AS]

  val hnil: HNil =
    IFix[ListLike[NilLike, ?], INil](NilLike)

  def hcons[A, AS <: Inductive](x: A, xs: AS): A :: AS =
    IFix[ListLike[A, ?], AS](ConsLike(x, xs))

  /**
    *  With that we can create proper heterogeneous lists, e.g.:
    */
  val hs: Int :: String :: HNil =
    hcons(1, hcons("bar", hnil))

  val is: Int :: String :: Int :: HNil =
    hcons(1, hcons("str", hcons(3, hnil)))
}
