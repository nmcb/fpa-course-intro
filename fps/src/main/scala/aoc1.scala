package aoc 

import scala.io.Source 

object AOC1 extends App:

  val depth = Source
        .fromFile("fps/src/main/resources/input01.txt").getLines.map(_.toInt).toList

  def increase(l: List[Int]): Int =
  l.sliding(2)
  .map {
    case List(x,y) => (x < y)
    case _         => println("boop")
  }
  .toList
  .map {
    case true  => 1
    case false => 0
  }
  .toList
  .sum

  println(s"The depth increases in ${increase(depth)} cases")   

  val increase3: Int = increase(depth.sliding(3).map(_.sum).toList)

  println("Depth increase in windows of three measurements occurs in " + increase3 + " cases")


  

