package fpa

import cats._
import cats.data._
import cats.effect._

object ErrorHandling:

  /** Functional errors */
  private sealed trait Deviation
  private case class RoomAlreadyExist() extends Deviation

  /** The monadic result stack */
  private type Result[A]  = Either[Deviation, A]
  private type ResultT[A] = EitherT[IO, Deviation, A]

  private val ResultT = EitherT

  private case class Room(id: Long)

  /** Some call that succeeds and wraps it result as: */
  private val callWhichSucceeds: ResultT[Room] =
    ResultT.pure[IO, Deviation](Room(666))

  /** Some call that deviates and wraps it result as: */
  private val callWhichDeviates: ResultT[Room] =
    ResultT.leftT[IO, Room](RoomAlreadyExist())

  /** Some call that throws and wraps it result as: */
  private val callWhichThrows: ResultT[Room] =
    MonadError[ResultT, Throwable].raiseError(new RuntimeException("Boom!"))


  private object http {

    /** Pretend a response is a http code and a body */
    private type Response = String

    /** Interface to "compute" a response string from a result and code */
    trait Http[A] {
      def asResponse(result: A): Int => IO[Response]
    }

    implicit def httpRoom: Http[Room] =
      (room: Room) => (code: Int) => IO(s"$code : $room")

    implicit def httpDeviation[E <: Deviation]: Http[E] =
      (error: E) => (code: Int) => IO(s"$code : ${error.toString}")

    implicit def httpResult[A : Http]: Http[Result[A]] =
      (result: Result[A]) => (code: Int) => result match {
      case Left(deviation) => deviation.toResponse(code)
      case Right(value)    => value.toResponse(code)
    }

    private case class ServerError(msg: String) extends Deviation

    implicit def httpResultT[A : Http]: Http[ResultT[A]] =
      (result: ResultT[A]) => (code: Int) => {
        val recovered = MonadError[ResultT, Throwable].recoverWith(result) {
          case t: Throwable => ResultT.leftT[IO, A](ServerError(t.getMessage))
        }
        recovered.value.flatMap(_.toResponse(code))
      }

    implicit class HttpOps[A : Http](underlying: A) {
      def toResponse(code: Int): IO[Response] =
        implicitly[Http[A]].asResponse(underlying)(code)
    }
  }

  import http._

  val prog: IO[Unit] = for {
    _ <- callWhichSucceeds.toResponse(201).map(println)
    _ <- callWhichDeviates.toResponse(200).map(println)
    _ <- callWhichThrows.toResponse(500).map(println)
  } yield ()

  import cats.effect.unsafe.implicits.global

  @main
  def runErrorHandling(args: String*): Unit =
    prog.unsafeRunSync()
