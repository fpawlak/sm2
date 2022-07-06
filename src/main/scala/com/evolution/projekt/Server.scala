package com.evolution.projekt

import doobie._
import doobie.implicits._
import doobie.util.ExecutionContexts

import cats._
import cats.data._
import cats.effect._
import cats.implicits._
import cats.syntax.either._

import org.http4s.{HttpApp, HttpRoutes, Response}
import org.http4s.server.blaze.BlazeServerBuilder
import org.http4s.dsl.io._
import org.http4s.implicits._

import scala.concurrent.ExecutionContext

import io.circe._
import io.circe.Decoder.Result
import io.circe.generic.JsonCodec
import io.circe.parser._
import io.circe.syntax._

import java.time.LocalDate
import scala.util.Try

object Server extends IOApp {
  private val serverRoutes = {
    object LocalDateVar {
      def unapply(value: String): Option[LocalDate] =
        Try(LocalDate.parse(value)).toOption
    }

    object GradeVar {
      def unapply(value: String): Option[Int] =
        Try(value.toInt).toOption.flatMap(i =>
          if(0 to 5 contains i) Some(i) else None)
    }

    import Domain._
    import io.circe.generic.auto._
    import org.http4s.circe.CirceEntityCodec._

    HttpRoutes.of[IO] {
      // all cards
      // curl "localhost:9001/cards"
      case GET -> Root / "cards" => Ok("hello")

      // cards for a certain date
      // curl "localhost:9001/cards/2022-07-06"
      case GET -> Root / "cards" / LocalDateVar(localDate) => Ok("hello")

      // adding a card
      // curl -XPOST "localhost:9001/addCard"
      case req @ POST -> Root / "addCard" => Ok("hello")

      // grading a card
      // curl -XPOST "localhost:9001/grade/100/5"
      case POST -> Root / "grade" / IntVar(cardId) / GradeVar(grade) => Ok("hello")
    }
  }

  val httpApp = serverRoutes.orNotFound

  override def run(args: List[String]): IO[ExitCode] =
    BlazeServerBuilder[IO](ExecutionContext.global)
      .bindHttp(port = 9001, host = "localhost")
      .withHttpApp(httpApp)
      .serve
      .compile
      .drain
      .as(ExitCode.Success)
}
