package fpa
package playground

// compiler prog down

sealed trait Functorial[F[_]] {
  def map[A,B](f: A => B)(F: F[A]): F[B]
}

// library prog down

sealed trait Optional[+A]
case object Niets        extends Optional[Nothing]
case class Iets[A](a: A) extends Optional[A]

object Optional {
  implicit val optionalFunctorial: Functorial[Optional] =
    new Functorial[Optional] {
      def map[A,B](f: A => B)(oi: Optional[A]): Optional[B] =
        oi match {
          case Niets   => Niets
          case Iets(a) => Iets(f(a))
        }
    }

  implicit class OptionalOps[A](oa: Optional[A])(implicit F : Functorial[Optional]) {
    def map[B](f: A => B): Optional[B] =
      F.map(f)(oa)
  }
}

sealed trait Multiple[+A]
case object Empty                           extends Multiple[Nothing]
case class Cell[A](a: A, rest: Multiple[A]) extends Multiple[A]

object Multiple {
  implicit val multipleFunctorial: Functorial[Multiple] =
    new Functorial[Multiple] {
      def map[A,B](f: A => B)(oi: Multiple[A]): Multiple[B] =
        oi match {
          case Empty   => Empty
          case Cell(a, r) => Cell(f(a), map(f)(r))
        }
    }

  implicit class MultipleOps[A](ma: Multiple[A])(implicit F : Functorial[Multiple]) {
    def map[B](f: A => B): Multiple[B] =
      F.map(f)(ma)
  }
}

object Test extends App {

}