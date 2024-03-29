package fpa

trait Monoid[A]:
  def emptyt(a: A): A

  def append(l: A)(r: A): A

  extension (l: A) def <>(r: A) =
    append(l)(r)

trait Functor[F[_]]:
  def map[A,B](f: A => B)(fa: F[A]): F[B]

  extension [A,B](f: A => B) def |@|(fa: F[A]): F[B] =
    map(f)(fa)

trait Applicative[F[_]](using val functor: Functor[F]):
  def pure[A](a: A): F[A]

  def ap[A,B](ff: F[A => B])(fa: F[A]): F[B]

  extension [A,B](ff: F[A => B]) def |*|(fa: F[A]): F[B] =
    ap(ff)(fa)

trait Monad[F[_]](using val applicative: Applicative[F]):
  def unit[A](a: A): F[A] =
    applicative.pure(a)

  def flatMap[A,B](fa: F[A])(f: A => F[B]): F[B]

  extension [A](fa: F[A]) def >>=[B](f: A => F[B]): F[B] =
    flatMap(fa)(f)

trait Foldable[F[_]]:
  def foldMap[A,B](f: A => B)(fa: F[A])(using monoid: Monoid[B]): B

trait Traversable[F[_]](using val functor: Functor[F], val foldable: Foldable[F]):
  def traverse[A,B,G[_]](f: A => G[B])(ga: F[A])(using applicative: Applicative[G]): G[F[B]]

enum Tree[+A]:
  case Leaf[A](a: A)                           extends Tree[A]
  case Node[A](val l: Tree[A], val r: Tree[A]) extends Tree[A]

object Tree:

  /** parses a recursive tuple-2 int tree, eg. ((1,(2,3)),(4,5)) =>
    * Node(Node(Leaf(1),Node(Leaf(2),Leaf(3))),Node(Leaf(4),Leaf(5)))
    */
  def parseIntTree(s: String): Tree[Int] =
    import P.*

    def leaf: P[Tree[Int]] =
      digits.map(Leaf.apply)

    def node: P[Tree[Int]] =
      for {
        _ <- char('(')
        l <- tree
        _ <- char(',')
        r <- tree
        _ <- char(')')
      } yield Node(l, r)

    def tree: P[Tree[Int]] =
      node | leaf

    tree.run(s)

  given Functor[Tree] with
    def map[A,B](f: A => B)(fa: Tree[A]): Tree[B] =
      fa match
        case Leaf(a)    => Leaf(f(a))
        case Node(l, r) => Node(f |@| l, f |@| r)

  given Applicative[Tree] with
    def pure[A](a: A): Tree[A] =
      Leaf(a)

    def ap[A, B](ff: Tree[A => B])(t: Tree[A]): Tree[B] =
      ff match
        case Leaf(f)    => f |@| t
        case Node(l, r) => Node(l |*| t, r |*| t)

  given Monad[Tree] with
    def flatMap[A, B](t: Tree[A])(f: A => Tree[B]): Tree[B] =
      t match
        case Leaf(a)    => f(a)
        case Node(l, r) => Node(l >>= f, r >>= f)

  given Foldable[Tree] with
    def foldMap[A,B](f: A => B)(fa: Tree[A])(using monoid: Monoid[B]): B =
      fa match
        case Leaf(a)    => f(a)
        case Node(l, r) => foldMap(f)(l) <> foldMap(f)(r)

  given Traversable[Tree] with
    def traverse[A,B,F[_]](f: A => F[B])(t: Tree[A])(using applicative: Applicative[F]): F[Tree[B]] =
      import applicative.functor.*
      t match
        case Leaf(a)    => Leaf.apply[B]         |@| f(a)
        case Node(l, r) => Node.apply[B].curried |@| traverse(f)(l) |*| traverse(f)(r)
