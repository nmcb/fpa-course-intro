package fpa
package playground

/** A synchronous poor man implementation of IO */
sealed trait IO[A] {

  import IO._

  @scala.annotation.tailrec def unsafePerformIO: A = this match {
    case Pure(a)    => a
    case Call(t)    => t().unsafePerformIO
    case Cont(v, f) => v match {
      case Pure(a)      => f(a).unsafePerformIO
      case Call(t)      => t().flatMap(f).unsafePerformIO
      case Cont(vv, ff) => vv.flatMap(a => ff(a).flatMap(f)).unsafePerformIO
    }
  }

  def flatMap[B](f: A => IO[B]): IO[B] =
    Cont(this, f)

  def map[B](f: A => B): IO[B] =
    flatMap(a => Pure(f(a)))
}

object IO {
  def suspend[A](io: => IO[A]): IO[A] = Call(() => io)
  def delay[A](a: => A): IO[A]        = Call(() => Pure(a))

  final case class Pure[A](a: A) extends IO[A]
  final case class Call[A](t: () => IO[A]) extends IO[A]
  final case class Cont[A, B](io: IO[A], f: A => IO[B]) extends IO[B]
}