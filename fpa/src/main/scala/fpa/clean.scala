package fpa
package clean

import scala.io.Source

object Main extends App:

  val clean =
    Source
      .fromFile("fpa/src/main/resources/demo.ml")
      .getLines
      .filterNot(_.startsWith("─("))
      .map(s => if (s.startsWith("utop")) "\n" + s else s)
      .toList

  clean.map(println)
