package fpa
package mapflatmap
package test

import org.scalatest._

class MapAndFlatMapSpec extends FlatSpec with Matchers {

  import library._

  "MapAndFlatMap" should "Q1: The naive map functions on options maps given function to its element" in {
    fmapOption[String, Int](_.toInt)(Some("1")) shouldBe Some(1)
    fmapOption[String, Int](_.toInt)(None) shouldBe None
  }

  it should "Q2: The naive map function on lists maps given function to its elements" in {
    fmapList[String, Int](_.toInt)(List("0", "1", "2", "3")) shouldBe List(0, 1, 2, 3)
    fmapList[String, Int](_.toInt)(Nil) shouldBe List.empty[Int]
  }

  it should "Q3: The `Map` type class have list and option instances in scope" in {
    implicitly[Map[List]].fmap[String, Int](_.toInt)(List("0", "1", "2", "3")) shouldBe List(0, 1, 2, 3)
    implicitly[Map[Option]].fmap[String, Int](_.toInt)(Option("1")) shouldBe Some(1)
  }

  it should "Q4: The `Map` syntax implicitly resolves on all types of kind `* -> *`" in {
    """ object client {
      |   val ls: List[String]   = List(0, 1, 2, 3).fmap(_.toString)
      |   val os: Option[String] = Option(1).fmap(_.toString)
      | }
    """.stripMargin should compile
  }

  it should "Q5: The `Map` type class allow instances for own data types" in {
    """
      | Pair(1, 2).fmap(_.toString) shouldBe Pair("1", "2")
    """.stripMargin should compile

    // And it should also work, so uncomment the line below as soon as it's compile ...
    // Pair(1, 2).fmap(_.toString) shouldBe Pair("1", "2")
  }

  it should "Q6: The `FlatMap` type class have list and option instances in scope" in {
    """
      | List("0", "1", "2", "3").bind(s => List(s.toInt)) shouldBe List(0, 1, 2, 3)
      | Option("0").bind(s => Some(s.toInt)) shouldBe Some(0)
    """.stripMargin should compile

    // And it should also work, so uncomment the lines below as soon as the above compiles ...
    // List("0", "1", "2", "3").bind(s => List(s.toInt)) shouldBe List(0, 1, 2, 3)
    // Option("0").bind(s => Some(s.toInt)) shouldBe Some(0)
  }

  it should "Q7: The `FlatMap` type class allows instances for new data type" in {
    """
      | Pair(1, 2).bind(i => Pair(i.toString, i.toString)) shouldBe Pair("1", "2")
    """.stripMargin should compile

    // And it should also work, so uncomment the line below as soon as the above compiles ...
    // Pair(1, 2).bind(i => Pair(i.toString, i.toString)) shouldBe Pair("1", "2")
  }

  it should "Q8: The `FlatMap` syntax implicitly resolves on all types of kind `* -> *`" in {
    """ object client {
      |   val ls: List[String]   = List(0, 1, 2, 3).bind(i => List(i.toString))
      |   val os: Option[String] = Option(1).bind(i => Some(i.toString))
      |   val pr: Pair[Int]      = Pair("1", "2").bind(s => Pair(s.toInt, s.toInt))
      | }
    """.stripMargin should compile
  }

  ignore should "Q9: (Bonus) The `FlatMap` and `Map` type classes encode a for comprehension" in {
    """
      | sealed trait Ref[A] {
      |   def value: A
      | }
      | case class Val[A](value: A) extends Ref[A]
      | case class Jmp[A](reference: () => Ref[A]) extends Ref[A] {
      |   def value: A = reference().value
      | }
      |
      | object Ref {
      |
      |   def unit[A](a: => A): Ref[A] =
      |     Val(a)
      |
      |   def jump[A](ref: => Ref[A]): Ref[A] =
      |     Jmp(() => ref)
      |
      |   implicit def refMap[A]: Map[Ref] = new Map[Ref] {
      |     def map[A, B](f: A => B)(ma: Ref[A]): Ref[B] = ma match {
      |       case Val(a) => unit(f(a))
      |       case Jmp(r) => unit(f(r().value))
      |     }
      |   }
      |
      |   implicit def refFlatMap[A]: FlatMap[Ref] = new FlatMap[Ref] {
      |     def flatMap[A, B](ma: Ref[A])(f: A => Ref[B]): Ref[B] = ma match {
      |       case Val(a) => f(a)
      |       case Jmp(r) => f(r().value)
      |     }
      |   }
      | }
      |
      | import Ref._
      | def mccarty91(n: Int): Ref[Int] =
      |   if (n > 100)
      |     unit(n - 10)
      |   else for {
      |     inner <- jump(mccarty91(n + 11))
      |     outer <- jump(mccarty91(inner))
      |   } yield outer
      |
      | (0 until 100).foreach {
      |   (i: Int) => mccarty91(i).value shouldBe 91
      | }
    """.stripMargin should compile

  }
}
