package fpa
package playground

import scala.util.Random

object FastInverseSquare:

  /**
   * Fast inverse sqrt implementation.
   *
   * @see [quake-iii](https://youtu.be/p8u_k2LIZyo)
   */

  import _root_.java.lang.Float.floatToIntBits
  import _root_.java.lang.Float.intBitsToFloat

  private val threehalfs: Float = 1.5f
  private val half: Float       = 0.5f

  private def fastInverseSquare(n: Float): Float =
    val x: Float = n * half
    var y: Float = n
    var i: Int   = floatToIntBits(y)
    
    i = 0x5f3759df - ( i >> 1 )
    y = intBitsToFloat(i)
    y = y * ( threehalfs - ( x * y * y ) )

    y

  private def mathInverseSquare(n: Float): Float =
    1.0f / scala.math.sqrt(n.toDouble).toFloat

  private val size   = 1000000
  private val rounds = 200
  private val ns     = for _ <- 1 to size   yield Random.nextFloat()
  private val test   = for t <- 1 to rounds yield
    
    val start1 = System.currentTimeMillis
    for n <- ns yield mathInverseSquare(n)    
    val start2 = System.currentTimeMillis
    for n <- ns yield fastInverseSquare(n)
    val stop = System.currentTimeMillis

    (start2 - start1, stop - start2)

  private def avg(xs: IndexedSeq[Long]): Double =
    xs.foldLeft(0L)(_ + _).toDouble / xs.length.toDouble

  @main
  def runFastInverseSquare(args: String*): Unit =
    val (mis,fis) = test.unzip
    println(s"avg: fastInverseSquare = mathInverseSquare * ${avg(fis) / avg(mis)}")

    // avg := fastInverseSquare = mathInverseSquare * 0.9388888888888889  // -- ie. less than 7% faster ;)



