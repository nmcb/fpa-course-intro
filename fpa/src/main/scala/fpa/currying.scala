package fpa

object currying extends App {
  
  def curry[A,B,C](f: (A, B) => C): A => B => C =
    (a: A) => (b: B) => f(a,b)

  val charAndStringToInt: (Char, String) => Int =
    (c: Char, s: String) => s.prepended(c).toInt

  assert(charAndStringToInt('1', "23") == 123)

  val charToStringToInt: Char => String => Int =
    curry(charAndStringToInt)

  assert(charToStringToInt('1')("23") == 123)

  val stringToInt: String => Int =
    charToStringToInt('1')

  assert(stringToInt("23") == 123)

  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  assert(
    uncurry(curry(charAndStringToInt))('1', "23") == charAndStringToInt('1', "23")
  )
}
