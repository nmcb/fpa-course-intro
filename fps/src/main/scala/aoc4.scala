package aoc
import scala.io.Source

object AOC4 extends App:

    val draws: List[Int] =
      Source
        .fromFile("fps/src/main/resources/input04.txt")
        .getLines
        .toList
        .head
        .split(",")
        .map(_.toInt)
        .toList

    val Line =
        """(\d+)\s+(\d+)\s+(\d+)\s+(\d+)\s+(\d+)""".r

    case class Board(rows: List[List[Int]]):
        def addRow(row: List[Int]): Board =
            copy(rows = rows :+ row)

        private def hasFilledRows(draws: List[Int]): Boolean =
            rows.exists(row => row.forall(a => draws.contains(a)))

        private def hasFilledColumns(draws: List[Int]): Boolean = 
            copy(rows.transpose).hasFilledRows(draws)
            
        def hasBingo(draws: List[Int]): Boolean =
            hasFilledRows(draws) || hasFilledColumns(draws)

        def score(draws: List[Int]): Int = 
            assert(hasBingo(draws))
            rows.flatten.filterNot(draws.contains).sum

    object Board:
        def empty: Board =
            Board(List.empty)

    def boards =
        Source
        .fromFile("fps/src/main/resources/input04.txt")
        .getLines
        .drop(2)
        .toList
        .map(_.trim)
        .foldLeft( (List.empty[Board], Board.empty) ) {
            case ((bs, b), "") =>
                ((b :: bs), Board.empty)
            case ((bs, b), Line(n0, n1, n2, n3, n4)) =>
                (bs, b.addRow(List(n0.toInt,n1.toInt,n2.toInt,n3.toInt,n4.toInt)))
            case ((_,_),_) =>
                sys.error("boom!")
        }._1

    def first(draws: List[Int]): (Board, List[Int]) =
        val succession: List[List[Int]] =
            draws.scanLeft(List.empty[Int])(_ :+ _)
        
        def found(draws: List[Int]): Option[Board] =
            boards.find(_.hasBingo(draws))

        val seq = succession.find(s => found(s).isDefined).get
        (found(seq).get, seq)

    val (b, ss) = first(draws)
    println(b.score(ss) * ss.last)