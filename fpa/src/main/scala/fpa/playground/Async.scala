package fpa
package playground

object Async:

  class Printer[A](val callback: (A => Unit) => Unit) extends Thread:

    override def run(): Unit =
      if (callback != null) callback(a => println(a.toString)) else ()
      Thread.sleep(100)
      run()

  object Printer:
    def apply[A](callback: (A => Unit) => Unit): Printer[A] =
      val printer = new Printer(callback)
      printer.start()
      printer


//  def runner(): Nothing = {
//    val counter = System.currentTimeMillis()
//    Printer.apply[Long] {
//      cb => cb(System.currentTimeMillis())
//    }
//    Thread.sleep(1000)
//    runner()
//  }
//
//  runner()

  import cats.effect._
  import scala.concurrent._
  import duration._

  private val program = IO.async[Int]: (callback: Either[Throwable, Int] => Unit) =>
    IO {
      import scala.concurrent.ExecutionContext.Implicits.global
      println("started!")
      val future = scala.concurrent.Future:
        Thread.sleep(5000)
        println("finished!")
      Await.result(future, Duration.Inf)
      callback(Right(1))
      Some(IO.unit)
    }

  import cats.effect.unsafe.implicits.global

  @main
  def runPrinter(args: String*): Unit =
    program.unsafeRunSync()
    println("prog ended!")
