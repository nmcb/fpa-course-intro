package fpa

object fixpoint {

  /*** We're going to implement a HList using fixed-points at type level ***/

  /*** But first ... Fixed Points at Value Level ***/

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
  lazy val facLib: Int => Int =
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
  val facUpToInfinity: Int => Int =
    facLike(facUpToInfinity) // Which never terminates [see line 39]

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
    *               Let's derive an (untyped) fix-combinator
    *
    *  fix(f) == fixpoint-of-f                       --by definition (our definition)
    *
    *  f(fixpoint-of-f) == fixpoint-of-f             --by definition (the definition)
    *
    *  fix(f) == fixpoint-of-f == f(fixpoint-of-f)   --by equality
    *
    *  fix(f) == f(fix(f))                           --by substitution
    *
    *  Voila!
    */


  /**
    *  Typed (fortunately) we need fix to accept a function:
    *
    *  a)  That takes a function of A to A to a function of A to A.
    *
    *  b)  So we can return a function from A to A.
    *
    *  Note that the input's function type is, itself, formed as the return type.
    */
  def fix[A]: ((A => A) => (A => A)) => (A => A) =
    (f: (A => A) => (A => A)) =>
      (a: A) =>
        f(fix(f))(a)

  /**
    *  In an untyped language this would just be:  def fix f = f fix f
    */


  /**
    *  So we compose the fixpoint of facLike, thus defining factorial!
    */
  val facFix: Int => Int =
    fix(facLike)



  /*** Fixed Points at Type Level : Let's build a heterogeneous list [1,2] ***/

  /*
   * Heterogeneous :
   *
   * [1]  Diverse in character or content.
   * [2]  Incommensurable through being of different kinds, degrees, or dimensions.
   */

  /**
    *  We start with defining a _type_ level fixpoint for some F.
    *  Here `F` is a type constructor of kind `* -> *` in which the
    *  type parameter represents function `F`'s input and the resulting
    *  `Fix[F[A]]` a fixpoint output "type".
    */
  case class Fix[F[_]](f: F[Fix[F]])

  /**
    *  And try to use that implementing a generic (non-recursive) list like ADT.
    */
  trait ListLike[+H, +T]
  case class ConsLike[H, +T](h: H, t: T) extends ListLike[H, T]
  trait NilLike extends ListLike[Nothing, Nothing]
  object NilLike extends NilLike

  /**
    *  With `Fix` we now are able to fix this "list like" ADT type's point,
    *  and means to encode the two constructor functions that allow us to
    *  construct a List in terms of a fixpoint calculation.
    */
  type List[A] =
    Fix[ListLike[A, *]]

  def nil[A]: List[A] =
    new Fix[ListLike[A, *]](NilLike)

  def cons[A](x: A, xs: List[A]): List[A] =
    new Fix[ListLike[A, *]](ConsLike(x, xs))

  /**
    *  With that we are already able to build lists at type level, e.g.
    */
  val xs = cons(1, cons(2, cons(3, nil)))

  /**
    *  Now onto fixpoint calculations at type level, we turn the rather
    *  easily defined recursive implementation of a `Fix`point data type
    *  above into a small ADT that will allow inductive expressions at
    *  type-level, while still encoding the `F[Fix[F]]` pattern as the
    *  encapsulated function `f`.  Mathematically we call this type level
    *  induction.
    */
  trait Inductive
  trait INil extends Inductive
  case class IFix[F[_], R <: Inductive](f: F[R]) extends Inductive

  /**
    *  Remember `case class Fix[F[_]](f: F[Fix[F]])`, well in `IFix` above
    *  we took the type application `Fix[F]` out of `f`s type, into a separate
    *  type parameter, `R`, which itself is bound by the "`Inductive`" coproduct.
    *  This allows for a inductive type sequence of inner `IFix`s as that inductive
    *  type constructor is recursively defined, and an additional inductive type
    *  constructor `INil` which we use to terminate the inductive recursion.
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
    IFix[ListLike[NilLike, *], INil]

  type ::[A, R <: Inductive] =
    IFix[ListLike[A, *], R]

  val hnil: HNil =
    IFix[ListLike[NilLike, *], INil](NilLike)

  def hcons[A, R <: Inductive](x: A, xs: R): A :: R =
    IFix[ListLike[A, *], R](ConsLike(x, xs))

  /**
    *  With that we can create proper heterogeneous lists, e.g.
    */
  val hs: Int :: String :: HNil = // <- look ma, cycling without hands!
    hcons(1, hcons("one", hnil))

  /*  =>  `Int :: String :: HNil`
   *  ==  `Int :: String :: IFix[ListLike[NilLike, *], INil]`
   *  ==  `Int :: IFix[ListLike[String, *], IFix[ListLike[NilLike, *], INil]]`
   *  ==  `IFix[ListLike[Int, *], IFix[ListLike[String, *], IFix[ListLike[NilLike, *], INil]]`
   */

  /**
    *  It's time to look back on what we've actually achieved at the end of this
    *  session.  We've crafted a type preserving version of the fix combinator,
    *  called `IFix` which accepts a type constructor `F` of kind `* -> *` as
    *  its "function" and calculates the fixpoint solution of the concrete type of
    *  that function during type checking.  Then we defined a non-recursive ADT
    *  describing the type-level structure of heterogeneous lists, and used the
    *  `IFix` representation to inductively calculate the type of many possible
    *  different expressions abiding that ADT _at_compile_time_!
    *
    *  The additional type alias language feature of scala provided us then with
    *  the means to create semantics, i.e. the `::` type returned by the `hcons`
    *  constructor, for this new heterogeneous type-level structure which
    *  integrates naturally with the existing scala language semantics.  E.g.:
    */
  val hellYeah: Seq[Int :: String :: HNil] =
    Seq(hs)
}
