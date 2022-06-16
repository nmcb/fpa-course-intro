package fpa
package bigraph

object Main extends App {

  // dependencies

  // type Set[A]    = scala.collection.Set[A]
  type Forest[N] = scala.collection.IndexedSeq[Tree[N]]


  // bigraphical

  sealed trait Foo
  case class Bar(s: String) extends Foo
  case class Baz(i: Int) extends Foo

  class Disjoint[A, B, C](val ac: A <:< C, val bc: A <:< C) {
    def union(as: Set[A], bs: Set[B]): Set[C] =
      as.asInstanceOf[Set[C]].union(bs.asInstanceOf[Set[C]])
  }

  implicit val fooDisjoint: Disjoint[Bar, Baz, Foo] =
    new Disjoint[Bar, Baz, Foo](implicitly, implicitly)

  println(fooDisjoint.union(Set(Bar("a"), Bar("b")), Set(Baz(1))))

  trait Tree[N]
  trait Graph[N, E]

    // categorical

  trait Obj[F[_]] {
    def identity: Arr[F, F]
  }
  trait Arr[F[_], G[_]] {
    def compose[H[_]](lhs: Arr[F, G])(rhs: Arr[G, H]): Arr[F, H]
    def dom: Obj[F]
    def cod: Obj[G]
  }

  implicit val setObj: Obj[Set] = new Obj[Set] {
    override def identity: Arr[Set, Set] = ???
  }

  trait Functor[F[_]] {
    def map[A, B](fa: F[A])(f: A => B): F[B]
  }

  implicit def setFunctor[A, B]: Functor[Set] = new Functor[Set] {
    override def map[A, B](fa: Set[A])(f: A => B): Set[B] =
      fa.map(f)
  }

  object examples {

  }
}
