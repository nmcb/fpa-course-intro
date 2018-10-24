package fpa

import cats._
import cats.data._
import cats.effect._

object monaderror extends App {

  /** Functional errors */
  sealed abstract class Deviation(msg: String) extends RuntimeException(msg)
  case class RoomAlreadyExist() extends Deviation("200 Ok")
  case class ServerError() extends Deviation(s"500 Server Error")

  /** Technical errors */
  case class CouldNotConnect() extends RuntimeException

  /** The monadic result stack */
  type Result[A]  = Either[Deviation, A]
  type ResultT[A] = EitherT[IO, Deviation, A]

  case class Room(id: Long)

  /** Some call that succeeds and wraps it result as: */
  val callWhichSucceeds: ResultT[Room] =
    EitherT.pure[IO, Deviation](Room(666))

  /** Some call that deviates and wraps it result as: */
  val callWhichDeviates: ResultT[Room] =
    EitherT.leftT[IO, Room](RoomAlreadyExist())

  /** Some call that throws and wraps it result as: */
  val callWhichThrows: ResultT[Room] =
    MonadError[ResultT[?], Throwable].raiseError(CouldNotConnect())


  object http {

    /** Pretend a response is a http code and a body */
    type Response = String

    /** Interface to "compute" a response string from a result and code */
    trait Http[A] {
      def asResponse(result: A): Int => IO[Response]
    }

    implicit def httpRoom: Http[Room] =
      (room: Room) => (code: Int) => IO(s"$code : $room")

    implicit def httpDeviation[E <: Deviation]: Http[E] =
      (error: E) => (code: Int) => IO(s"$code : ${error.getMessage}")

    implicit def httpResult[A : Http]: Http[Result[A]] =
      (result: Result[A]) => (code: Int) => result match {
      case Left(deviation) => deviation.toResponse(code)
      case Right(value)    => value.toResponse(code)
    }

    implicit def httpResultT[A : Http]: Http[ResultT[A]] =
      (result: ResultT[A]) => (code: Int) => {
        val recovered = MonadError[ResultT[?], Throwable].recoverWith(result) {
          case t: Throwable => EitherT.leftT[IO, A](ServerError())
        }
        recovered.value.flatMap(_.toResponse(code))
      }

    implicit class HttpOps[A : Http](underlying: A) {
      def toResponse(code: Int): IO[Response] =
        implicitly[Http[A]].asResponse(underlying)(code)
    }
  }

  import http._

  val prog = for {
    _ <- callWhichSucceeds.toResponse(201).map(println)
    _ <- callWhichDeviates.toResponse(200).map(println)
    _ <- callWhichThrows.toResponse(500).map(println)
  } yield ()

  prog.unsafeRunSync
}