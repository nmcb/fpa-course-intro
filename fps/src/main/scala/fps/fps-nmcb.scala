package fps

// compiler programmer code down below

sealed trait Functorial[F[_]]:
  def map[A,B](f: A => B)(F: F[A]): F[B]

// library programmer code down below

sealed trait Optional[+A]
case object Non          extends Optional[Nothing]
case class  The[A](a: A) extends Optional[A]

object Optional:
  
  given Functorial[Optional] =
    new Functorial[Optional]:
      def map[A,B](f: A => B)(oi: Optional[A]): Optional[B] =
        oi match
          case Non    => Non
          case The(a) => The(f(a))

  extension [A](oa: Optional[A])(using F : Functorial[Optional])
    def map[B](f: A => B): Optional[B] =
      F.map(f)(oa)

sealed trait Multiple[+A]
case object Empty                           extends Multiple[Nothing]
case class Cell[A](a: A, rest: Multiple[A]) extends Multiple[A]

object Multiple:

  private def empty[A]: Multiple[A] =
    Empty

  def apply[A](as: A*): Multiple[A] =
    as.foldRight(Multiple.empty)((elm,acc) => Cell(elm, acc))

  given Functorial[Multiple] =
    new Functorial[Multiple]:
      def map[A,B](f: A => B)(oi: Multiple[A]): Multiple[B] =
        oi match
          case Empty      => Empty
          case Cell(a, r) => Cell(f(a), map(f)(r))

  extension [A](ma: Multiple[A])(using F : Functorial[Multiple])
    def map[B](f: A => B): Multiple[B] =
      F.map(f)(ma)

// application code example usage down below

object Test:

  private case class Person(name: String, age: Int)

  private val fixture: Seq[Person] =
    Seq(
      Person("John Doe", 42),
      Person("Jane Doe", 43),
      Person("Jake Joe", 41)
    )

  @main
  def run(args: String*): Unit =
    assert(fixture.find(_.age == 41).contains(Person("Jake Joe", 41)))
    assert(!fixture.exists(_.age == 40))
