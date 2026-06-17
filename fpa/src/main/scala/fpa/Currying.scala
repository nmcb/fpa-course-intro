package fpa

object Currying:
  
  def curry[A,B,C](f: (A, B) => C): A => B => C =
    (a: A) => (b: B) => f(a,b)

  val charAndStringToInt: (Char, String) => Int =
    (c: Char, s: String) => s.prepended(c).toInt

  val charToStringToInt: Char => String => Int =
    curry(charAndStringToInt)
  
  val stringToInt: String => Int =
    charToStringToInt('1')
  
  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  @main
  def run(args: String*): Unit =
    assert(charAndStringToInt('1', "23") == 123)
    assert(charToStringToInt('1')("23") == 123)
    assert(stringToInt("23") == 123)
    assert(uncurry(curry(charAndStringToInt))('1', "23") == charAndStringToInt('1', "23"))
