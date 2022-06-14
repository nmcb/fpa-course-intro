package fpa
package playground

import scala.util.Random

object FastInverseSquare extends App {

  // fast inverse sqrt implementation - https://youtu.be/p8u_k2LIZyo

  import java.lang.Float.floatToIntBits
  import java.lang.Float.intBitsToFloat

  val threehalfs: Float = 1.5f
  val half: Float       = 0.5f

  def fisqrt(n: Float): Float = {

    val x: Float = n * half
    var y: Float = n
    var i: Int   = floatToIntBits(y)
    
    i = 0x5f3759df - ( i >> 1 )
    y = intBitsToFloat(i)
    y = y * ( threehalfs - ( x * y * y ) )

    y
  }

  def misqrt(n: Float): Float =
    1.0f / scala.math.sqrt(n.toDouble).toFloat

  val size   = 1000000
  val rounds = 200
  val ns     = for (_ <- 1 to size)   yield Random.nextFloat()
  val test   = for (t <- 1 to rounds) yield {
    
    val start1 = System.currentTimeMillis
    for (n <- ns) yield misqrt(n)    
    val start2 = System.currentTimeMillis
    for (n <- ns) yield fisqrt(n)
    val stop = System.currentTimeMillis

    (start2 - start1, stop - start2)
  }

  def avg(xs: IndexedSeq[Long]): Double =
    xs.foldLeft(0L)(_ + _).toDouble / xs.length.toDouble

  val (mis,fis) = test.unzip
  println(s"avg: fisqrt = misqrt * ${avg(fis) / avg(mis)}")

  // avg := fisqrt = misqrt * 0.9388888888888889  // -- ie. less than 7% faster ;)
}



