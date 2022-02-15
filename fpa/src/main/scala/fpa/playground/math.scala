package fpa
package playground

import scala.util.Random

object math extends App {

  // fast inverse sqrt implementation - https://youtu.be/p8u_k2LIZyo

  import java.lang.Float.floatToIntBits
  import java.lang.Float.intBitsToFloat

  def isqrt(n: Float): Float = {

    val threehalfs: Float = 1.5f
    val half: Float       = 0.5f

    val x: Float = n * half
    var y: Float = n
    var i: Int   = floatToIntBits(y)
    
    i = 0x5f3759df - ( i >> 1 )
    y = intBitsToFloat(i)
    y = y * ( threehalfs - ( x * y * y ) )

    y
  }

  def disqrt(n: Float): Float =
    1.0f / scala.math.sqrt(n.toDouble).toFloat

  val size   = 10000000
  val rounds = 200
  val ns     = for (i <- 1 to size) yield Random.nextFloat
  val test   = for (t <- 1 to rounds) yield {
    
    val start1 = System.currentTimeMillis
    for (n <- ns) yield disqrt(n)    
    val start2 = System.currentTimeMillis
    for (n <- ns) yield isqrt(n)
    val stop = System.currentTimeMillis

    println(s"ds[$t]=${stop - start1}ms - di[$t]=${stop - start2}ms")

    (stop - start1, stop - start2)
  }

  def avg(xs: IndexedSeq[Long]): Double = {
    val (sum, length) = xs.foldLeft((0L,0L))( { case ((s,l),x)=> (x+s,1+l) })
    sum / length
  }

  val (dss,dis) = test.unzip
  println(s"avg: di = ds * ${avg(dis) / avg(dss)}")

  // avg: di = ds * 0.5285714285714286
}



