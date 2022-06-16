package fpa
package playground

object contra {

  sealed trait Opt[+A] {

    def flatMap[B](f: A => Opt[B]): Opt[B] =
      this match { case Non => Non ; case The(a) => f(a) }

    def map[B](f: A => B): Opt[B] =
      flatMap(a => The(f(a)))

    def getOrElse[B >: A](that: => B): B =
      this match { case Non => that ; case The(a) => a }
  }    

  object Opt {

    def empty[A]: Opt[A] =
      Non

    def apply[A](a: A): Opt[A] =
      The(a)
  }

  private case class   The[A](a: A) extends Opt[A]
  private case object  Non          extends Opt[Nothing]
} 