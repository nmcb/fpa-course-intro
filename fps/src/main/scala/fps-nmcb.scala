package fps
package nmcb

// compiler programmer code down below

sealed trait Functorial[F[_]]:
  def map[A,B](f: A => B)(F: F[A]): F[B]

// library programmer code down below

sealed trait Optional[+A]
case object Non          extends Optional[Nothing]
case class  The[A](a: A) extends Optional[A]

object Optional:
  implicit val optionalFunctorial: Functorial[Optional] =
    new Functorial[Optional]:
      def map[A,B](f: A => B)(oi: Optional[A]): Optional[B] =
        oi match
          case Non   => Non
          case The(a) => The(f(a))

  implicit class OptionalOps[A](oa: Optional[A])(implicit F : Functorial[Optional]):
    def map[B](f: A => B): Optional[B] =
      F.map(f)(oa)

sealed trait Multiple[+A]
case object Empty                           extends Multiple[Nothing]
case class Cell[A](a: A, rest: Multiple[A]) extends Multiple[A]

object Multiple:
  implicit val multipleFunctorial: Functorial[Multiple] =
    new Functorial[Multiple]:
      def map[A,B](f: A => B)(oi: Multiple[A]): Multiple[B] =
        oi match
          case Empty   => Empty
          case Cell(a, r) => Cell(f(a), map(f)(r))

  implicit class MultipleOps[A](ma: Multiple[A])(implicit F : Functorial[Multiple]):
    def map[B](f: A => B): Multiple[B] =
      F.map(f)(ma)

// application programmerrcode ammer down below

object Test extends App:

  case class Person(name: String, age: Int)

  val fixture: Seq[Person] =
    Seq(
      Person("John Doe", 42),
      Person("Jane Doe", 43),
      Person("Jolly Joe", 41)
    )

  assert(fixture.find(_.age == 41) == Some(Person("Jolly Joe", 41)))
  assert(fixture.find(_.age == 40) == None)




  def curry[A,B,C](f: (A, B) => C): A => B => C =
    (a: A) => (b: B) => f(a,b)

  val mf: (Char, String) => Int =
    (c: Char, s: String) => s.prepended(c).toInt

  assert(mf('1', "23") == 123)
  val curriedmf = curry(mf)
  val pamf = curry(mf)('1')
  assert(pamf("23") == 123)

  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  assert(mf('1', "23") == uncurry(curriedmf)('1', "23"))
