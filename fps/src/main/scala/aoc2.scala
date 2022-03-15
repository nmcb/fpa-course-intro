package aoc 

import scala.io.Source


object AOC2 extends App:

  val move = 
    Source
    .fromFile("fps/src/main/resources/testinput.txt")
    .getLines
    .toList

  
  def horizontal(l: List[String]): List[String] = 
    l.filter(_.startsWith("forward"))

    // want to extract the values and add them together

    

  // def vertical(l: List[String]): List[String] = 
  //  l.filter(_.startsWith("up")) :: l.filter(_.startsWith("down"))

  // current thoughts 

  // can split in up (neg) and down (pos) and hor (pos)
  // pattern match on eg. 'down ', extract the int that follows and add to an accumulator?
  

    

  println(horizontal(move))