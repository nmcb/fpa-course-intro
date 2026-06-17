package fpa

// A version of the archetypical, introduction "Hello World" example in Scala

object Intro:

  object Main extends Greeting:

    def runIntro(args: String*): Unit =
      println(greeting)


  trait Greeting:
    lazy val greeting: String = "Hello Anonymous Functional Programmer !!!"
