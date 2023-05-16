package fpa

object GADT extends App:

  enum Expr[A]:
    case Var[A](a: A)                               extends Expr[A]
    case Lit(n: Int)                                extends Expr[Int]
    case Plus(lhs: Expr[Int], rhs: Expr[Int])       extends Expr[Int]
    case Fun[A, B](fun: Expr[A] => Expr[B])         extends Expr[A => B]
    case App[A, B](fun: Expr[A => B], arg: Expr[A]) extends Expr[B]

  import Expr.*

  def eval[A](e: Expr[A]): A =
    e match
      case Var(a)        => a
      case Lit(i)        => i
      case Plus(l, r)    => eval(l) + eval(r)
      case f: Fun[a, b]  => (x: a) => eval(f.fun(Var(x)))
      case App(fun, arg) => eval(fun)(eval(arg))

  // Why does type checking the eval function require special reasoning?
  //
  // First, in all but the Var case, the type of the scrutinee e is
  // refined from Expr[A] to a more precise type. For example, Lit
  // extends Expr[Int], so if e matches Lit(i), we can deduce that
  // A = Int. This allows i, which has type Int, to agree with eval’s
  // expected return type A.
  //
  // Second, in addition to refining A, the Fun and App cases uncover
  // existential types (unknown types that do not appear in the
  // function’s signature); in the App case, which matches against
  // patterns of type App[X,A] <: Expr[A], type X is unknown, but has
  // to be treated consistently as it appears in the two extracted
  // subexpressions fun and arg. In the Fun case, we have to use a type
  // pattern f: Fun[a,b] to bind the uncovered existential type a so we
  // can use it in a required type annotation.
  //
  // Declaration-Site Variance - Ie. Open GADTs
  //
  // Scala supports declaration-site variance, a convenient way of
  // defining subtyping relationships between parameterized types.
  // For instance, in Figure 1 we could make Expr covariant to encode
  // the λ calculus with subtyping. However, Scala then rejects our
  // definition of eval as ill-typed, and requires adding unsafe casts.
  //
  // It is unclear whether definitions like eval actually are unsafe,
  // or whether Scala is overly conservative; indeed, sound typing rules
  // for pattern matching on open GADTs is an open problem!

