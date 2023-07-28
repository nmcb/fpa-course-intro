package fpa

import org.scalatest.*
import flatspec.*
import matchers.should.*

class AFPTest extends AnyFlatSpec with Matchers:

  import Tree.*
  import Tree.given

  val fixture: Tree[Int] =
    parseIntTree("((1,(2,3)),(4,5))")

  "Tree.parseIntTree" should "parse int trees" in {
    fixture should be(
      Node(Node(Leaf(1),Node(Leaf(2),Leaf(3))),Node(Leaf(4),Leaf(5)))
    )
  }

  "given Functor[Tree]" should "map functions over trees" in {
    ((_: Int) + 1) |@| fixture should be(
      Node(Node(Leaf(2),Node(Leaf(3),Leaf(4))),Node(Leaf(5),Leaf(6)))
    )
  }

  "given Applicative[Tree]" should "apply functions to trees" in {
    val functions: Tree[Int => Int] = Leaf(_ + 1)

    functions |*| fixture should be(
      Node(Node(Leaf(2), Node(Leaf(3), Leaf(4))), Node(Leaf(5), Leaf(6)))
    )
  }
