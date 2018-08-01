package fpa
package mapflatmap

/** This chapter is intended to explain what is generic about the application
  * of the `map` and `flatMap` functions when defined for all the different
  * types that support them.  You'll probably know for example that the
  * `Option[A]` and `List[A]` types have a Scala library implementation of
  * `map` and `flatMap`, but these will not be the implementations we'll be
  * focusing on or even be using today.
  *
  * We'll instead be focusing on abstracting over all the types that support
  * `map` and `flatMap`, and define that functionality ... as type classes.
  *
  * For this reason you should treat the Scala `List[A]` and `Option[A]`
  * classes as simple data constructors only, while doing the exercises in this
  * file.  I.e. only consider the cons constructor `::` and the empty list
  * constructor `Nil` for the `List[A]` type, and the absent value constructor
  * `None` and the present value constructor `Some` for the `Option[A]` type.
  *
  * Your answers should not use the available Scala library functions `map`
  * and `flatMap` already defined on `List[A]` and `Option[A]` as we're going
  * to implement these functions ourselves.  In order to avoid name clashes
  * at the client's call site we'll be using the more traditional names `fmap`
  * for `map`, and `bind` (or its symbolic operator `>>=`) for `flatMap` in
  * our own implementations. Up till the last bonus question, which requires
  * us to rename them to `map` and `flatMap` as this is required by Scala's
  * for comprehension semantics.
  */
object library {

  // Implementation Notes:
  // - Map is categorically defined as     :: (a -> b) -> m a -> m b
  // - FlatMap is categorically defined as :: m a -> (a -> m b) -> m b

  /** Q1: Implement a naive map on options with a pattern match on `oa`. */
  def fmapOption[A, B](f: A => B)(oa: Option[A]): Option[B] =
    ???

  /** Q2: Implement a naive map on lists with a pattern match on `la`. */
  def fmapList[A, B](f: A => B)(la: List[A]): List[B] =
    ???

  trait Map[M[_]] {
    def fmap[A, B](f: A => B)(ma: M[A]): M[B]
  }

  /** Q3: Implement an option and list instance for the `Map` type class. */
  object Map {
    implicit val listMap: Map[List] =
      ???

    implicit val optionMap: Map[Option] =
      ???
  }

  /** Q4: Create a syntax for the `Map` type class. */
  // Hint: you want an implicit class that wraps a generic `M[A]` type class
  // instance.  These instances are of kind `* -> *` which means that they
  // take a type to become a concrete type.  Or in other words, they are
  // are type constructors.  `List[A]` and `Option[A]` are both type
  // constructors as they take one concrete type for type parameter `A`.
  //
  // `implicit class MapOps[M[_], A]` ...

  /** Q5: Implement a `Map` instance for your own data type */
  case class Pair[A](a1: A, a2: A)
  object Pair {
    implicit val pairMap: Map[Pair] =
      ???
  }

  /** Q6 - Q8: Implement the same functionality for type class `FlatMap`. */
  trait FlatMap[M[_]] {
    def bind[A, B](ma: M[A])(f: A => M[B]): M[B]
  }

  object FlatMap {
    implicit val listFlatMap: FlatMap[List] =
      ???

    implicit val optionFlatMap: FlatMap[Option] =
      ???

    implicit val pairFlatMap: FlatMap[Pair] =
      ???
  }

  // `implicit class FlatMapOps[M[_], A]` ...

  /** Q9: (Bonus)
    *
    * Now rename the `fmap` and `bind` functions in both type classes, instances
    * and syntax into `map` and `flatMap`.  Make sure you also rename them in
    * the code strings in tests Q4 and Q8.  After that enable Q9 and make it
    * compile.
    */
}