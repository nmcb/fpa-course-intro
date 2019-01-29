package fpa.playground

object Main extends App {

  class Printer[A](val callback: (A => Unit) => Unit) extends Thread {
    override def run(): Unit = {
      if (callback != null) callback(a => println(a.toString)) else ()
      Thread.sleep(100)
      run()
    }
  }
  object Printer {
    def apply[A](callback: (A => Unit) => Unit): Printer[A] = {
      val printer = new Printer(callback)
      printer.start()
      printer
    }
  }


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
  val prog = IO.async[Unit] { cb => {
    import scala.concurrent.ExecutionContext.Implicits.global
    println("started!")
    scala.concurrent.Future {
      Thread.sleep(5000)
      println("finished!")
    }
    // Await.result(future, Duration.Inf)
    cb(Right(()))
  }}
  prog.unsafeRunSync()
  println("prog ended!")
}
