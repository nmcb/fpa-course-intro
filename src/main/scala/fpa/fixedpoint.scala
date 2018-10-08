package fpa

object fixpoint extends App {

  /*** Fixed Points at Value Level ***/

  /**
    *  Let us try to factor out the recursive part of a function.
    */
  val facRec: Int => Int =
    (n: Int) =>
      if (n == 0) 1 else n * facRec(n - 1)

  /**
    *  Replace the recursive call with a higher order function parameter.
    */
  val facLike: (Int => Int) => (Int => Int) =
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
    facLike(facLib)  // Proof by substitution.

  /**
    *  Thus, could we define factorial as a recursive curried parameter?
    */
  val alsoFac2: Int => Int =
    (n: Int) =>
      facLike(alsoFac2)(n) // Yes! (But we need a lambda). [see line 69]

  /**
    *  Let's define a utility method.
    */
  def id[A]: A => A =
    a => a  // Identity.

  /**
    *  Which makes it possible to calculate factorial for n <= 0
    */
  val facUpTo0: Int => Int =
    facLike(id)  // And ... without a recursive call!

  /**
    *  Therefor we could "stack" the recursion explicitly for n <= 1.
    */
  val facUpTo1: Int => Int =
    facLike(facUpTo0)  // Note, still no recursive call!

  /**
    *  And for n <= 2 ... etc., going up towards infinity.
    */
  val facUpTo2: Int => Int =
    facLike(facUpTo1)

  /**
    *  We could now "conceptually" define the fixpoint of facLike as:
    */
  val factorialUpToInfinity: Int => Int =
    facLike(factorialUpToInfinity) // Which never terminates [see line 39]

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
    *  Let's derive a function that calculates a function fixpoint, e.g.:
    *
    *               Let's derive the (untyped) Y-Combinator
    *
    *  Y(f) == fixpoint-of-f                       --by definition (our definition)
    *
    *  f(fixpoint-of-f) == fixpoint-of-f           --by definition (the definition)
    *
    *  Y(f) == fixpoint-of-f == f(fixpoint-of-f)   --by equality
    *
    *  Y(f) == f(Y(f))                             --by substitution
    *
    *  Voila!
    */


  /**
    *  Typed (fortunately) we need Y to accept a function:
    *
    *  a)  That takes a function of A to A to a function of A to A.
    *
    *  b)  So we can return a function from A to A.
    *
    *  Note that the input's function type is, itself, formed as the return type.
    */
  def Y[A]: ((A => A) => (A => A)) => (A => A) =
    (f: (A => A) => (A => A)) =>
      (a: A) =>
        f(Y(f))(a)

  /**
    *  So we compose the fixpoint of facLike, thus defining factorial!
    */
  val fixFac: Int => Int =
    Y(facLike)



  /*** Fixed Points at Type Level ***/

  import scala.language.higherKinds

  /**
    *  Let's start with defining a _type_ level fixpoint for some F.
    */
  case class Fix[F[_]](f: F[Fix[F]])

  /**
    *  And try to use that implementing a generic (non-recursive) List-ADT.
    */
  trait ListLike[+A, +LL]
  case class ConsLike[A, +LL](a: A, as: LL) extends ListLike[A, LL]
  trait NilLike extends ListLike[Nothing, Nothing]
  object NilLike extends NilLike

  /**
    *  This means we need to be able to fix that ADT type's point and
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
    *  Remember `case class Fix[F[_]](f: F[Fix[F]])`, well in `IFix` above
    *  we took the type application `Fix[F]` out of `f`s type, into a separate
    *  type parameter, `R`, which itself inherits from an "`Inductive`" sequence
    *  of inner `IFix`s, that terminate in a type level `INil`.
    */

  /**
    *  Note 1)  Since R is of kind `*` we have recursion at the type level. So
    *           contrarily to Fix, in IFix not every element in the recursion
    *           needs to be of the same type.
    *
    *  Note 2)  At some point we'll need to stop the recursion. The INil
    *           will have no inhabitant (inherited class or object) and
    *           just serves for that purpose.
    */

  /**
    *  Now how does this help us implementing a HList?  Well, here it becomes
    *  really abstract.  You see, the only difference between a List and a
    *  HList is the recursion scheme.  A HList is recursive at both the type and
    *  value level at the same time, while a List is only recursive at the value
    *  level.  Apart from that, they are the same.  Therefore, we can reuse the
    *  previously defined NilLike and ConsLike, and we just have to provide new
    *  constructors.
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
    hcons(1, hcons("one", hnil))

  val yeah: Seq[Int :: String :: HNil] = // <- look mama, without hands!
    Seq(hs)

  println(alsoFac2(3))
}
