package fpa
package intro

// A version of the archetypical introductionary Hello World code

object Hello extends Greeting with App {
  println(greeting)
}

trait Greeting {
  lazy val greeting: String = "Hello Anonymous Functional Programmer !!!"
}
