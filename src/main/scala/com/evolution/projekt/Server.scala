package com.evolution.projekt

import doobie._
import doobie.implicits._
import doobie.util.ExecutionContexts
import doobie.implicits.legacy.localdate._

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

    val xa = Transactor.fromDriverManager[IO](
      "org.postgresql.Driver",     // driver classname
      "jdbc:postgresql:testdb",     // connect URL (driver-specific)
      "postgres",                  // user
      "password"                           // password
    )

    HttpRoutes.of[IO] {
      // all cards
      // curl "localhost:9001/cards"
      case GET -> Root / "cards" => for {
        list <- sql"SELECT * FROM cards".query[Card].to[List].transact(xa)
        res <- Ok(list)
      } yield res

      // cards for a certain date
      // curl "localhost:9001/cards/2022-07-06"
      case GET -> Root / "cards" / LocalDateVar(localDate) => for {
        list <- sql"SELECT * FROM cards WHERE scheduledfor = $localDate".query[Card].to[List].transact(xa)
        res <- Ok(list)
      } yield res

      // adding a card
      // curl -XPOST "localhost:9001/addCard" -d '{"qaQuestion":"pytanie","qaAnswer":"odpowiedz","qaScheduledFor":"2022-07-07"}' -H "Content-Type: application/json"
      case req @ POST -> Root / "addCard" =>
        req.as[QuestionAnswer].flatMap { qAndA =>
          val question = qAndA.qaQuestion
          val answer = qAndA.qaAnswer
          val repetitions = 0
          val ef: Double = 2.5
          val interval: Long = 0
          val scheduledFor = qAndA.qaScheduledFor
          val insertQuery: ConnectionIO[Int] = sql"""
               | INSERT INTO cards 
               | (question, answer, repetitions, ef, interval, scheduledfor) 
               | VALUES 
               | ($question, $answer, $repetitions, $ef, $interval, $scheduledFor)
               |""".stripMargin.update.withUniqueGeneratedKeys("cardid")
          for {
            cardId <- insertQuery.transact(xa)
            res <- Created(cardId)
          } yield res
        }

      // grading a card
      // curl -XPOST "localhost:9001/grade/100/5"
      case POST -> Root / "grade" / IntVar(cardId) / GradeVar(grade) => for {
        oldCardOpt <- sql"SELECT * FROM cards WHERE cardid = $cardId".query[Card].option.transact(xa)
        res <- oldCardOpt match {
          case None => NotFound()
          case Some(oldCard) => {
            val newCard = Sm2.rate(oldCard, grade)
            sql"""
                  | UPDATE cards SET 
                  | repetitions = ${newCard.repetitions}, 
                  | ef = ${newCard.ef}, 
                  | interval = ${newCard.interval}, 
                  | scheduledfor = ${newCard.scheduledFor} 
                  | WHERE cardid = $cardId
                  |""".stripMargin.update.run.transact(xa) >> Ok()
          }
        }
      } yield res
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
