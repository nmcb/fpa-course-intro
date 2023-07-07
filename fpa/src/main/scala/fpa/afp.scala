package afp

enum Tree[+A]:
  case Leaf[A](a: A)                           extends Tree[A]
  case Node[A](val l: Tree[A], val r: Tree[A]) extends Tree[A]

trait Functor[F[_]]:
  def fmap[A,B](f: A => B)(fa: F[A]): F[B]

  extension [A,B](f: A => B)
    def |@|(fa: F[A]): F[B] =
      fmap(f)(fa)

trait Applicative[F[_] : Applicative]:
  def pure[A](a: A): F[A]

  def ap[A, B](ff: F[A => B])(fa: F[A]): F[B]

  extension [A,B](ff: F[A => B])
    def |~|(fa: F[A]): F[B] =
      ap(ff)(fa)


object Tree:

  given Functor[Tree] with
    def fmap[A,B](f: A => B)(fa: Tree[A]): Tree[B] =
      fa match
        case Leaf(a)    => Leaf(f(a))
        case Node(l, r) => Node(f |@| l, f |@| r)

//  trait Functor[F[_]]:
//    extension [A](fa: F[A])
//      def fmap[B](f: A => B): F[B]
//
//  trait Applicative[F[_]] extends Functor[F]:
//    extension [A](fa: F[A])
//      def pure(a: A): F[A]
//      def <*>[B](gf: F[A => B]): F[B]
//      def map2[B, C](fb: F[B])(f: (A, B) => C): F[C] = fb <*> fa.fmap(f.curried)
//
//  trait Monad[F[_]] extends Applicative[F]:
//    extension [A](fa: F[A])
//      def unit(a: A): F[A]
//      def flatMap[B](f: A => F[B]): F[B]
//
//  trait Monoid[A]:
//    def mzero: A
//    def mappend(a1: A, a2: A): A
//
//  trait Foldable[F[_]]:
//    extension [A](fa: F[A])
//      def foldMap[B](m: Monoid[B])(f: A => B): B
//
//  trait Traversable[F[_]] extends Functor[F] with Foldable[F]:
//    extension [A](fa: F[A])
//      def traverse[G[_]: Applicative, B](f: A => G[B]): G[F[B]]
//
//  enum Tree[+A]:
//    case Leaf(a: A)
//    case Node(l: Tree[A], r: Tree[A])
//
//  object Tree:
//
//    given Functor[Tree] with
//      extension [A](fa: Tree[A])
//        def fmap[B](f: A => B): Tree[B] = fa match
//          case Leaf(a)    => Leaf(f(a))
//          case Node(l, r) => Node(l.fmap(f), r.fmap(f))
//
//    given Applicative[Tree] with
//      extension [A](fa: Tree[A])
//        def pure(a: A): Tree[A] = Leaf(a)
//        def <*>[B](gf: Tree[A => B]): Tree[B] = gf match
//          case Leaf(f)    => fa.fmap(f)
//          case Node(l, r) => Node(fa <*> l, fa <*> r)
//
//    given Monad[Tree] with
//      extension [A](fa: Tree[A])
//        def unit(a: A): Tree[A] = Leaf(a)
//        def flatMap[B](f: A => Tree[B]): Tree[B] = fa match
//          case Leaf(v)    => f(v)
//          case Node(l, r) => Node(l.flatMap(f), r.flatMap(f))
//
//    val stringMonoid: Monoid[String] = new Monoid[String]:
//      override def mappend(a1: String, a2: String): String = a1 + a2
//      override def mzero: String = ""
//    given Foldable[Tree] with
//      extension [A](fa: Tree[A])
//        def foldMap[B](m: Monoid[B])(f: A => B): B = fa match
//          case Leaf(v)    => f(v)
//          case Node(l, r) => m.mappend(l.foldMap(m)(f), r.foldMap(m)(f))
//
//    // cannot find the reason why below doesn't compile
//    given Traversable[Tree] with
//      extension [A](fa: Tree[A])
//        def traverse[G[_]: Applicative, B](f: A => G[B]): G[Tree[B]] = fa match
//          case Leaf(v)    => f(v).fmap(Leaf.apply)
//          case Node(l, r) =>
//            val q = l.traverse(f) <*> r.traverse(f)
//            ???