package com.evolution.projekt

import cats._
import cats.data._
import cats.effect._
import cats.implicits._
import scala.util.{Try,Success,Failure}

import org.http4s._
import org.http4s.client.{Client => Http4sClient}
import org.http4s.client.blaze.BlazeClientBuilder
import org.http4s.client.dsl.io._
import org.http4s.dsl.io._
import org.http4s.headers._
import org.http4s.implicits._

import scala.concurrent.ExecutionContext
import java.time.LocalDate
import Domain.QuestionAnswer

object Client extends IOApp {

  object Console {
    def putStrLn(value: String): IO[Unit] = IO(println(value))
    val readLn: IO[String] = IO(scala.io.StdIn.readLine())
  }

  import Console._

  import io.circe.generic.auto._
  import org.http4s.circe.CirceEntityCodec._

  private val uri = uri"http://localhost:9001"

  val selectAction: IO[Int] = for {
    _ <- putStrLn("Select action:")
    _ <- putStrLn("1 - List all cards by date")
    _ <- putStrLn("2 - Review cards scheduled for today")
    _ <- putStrLn("3 - Review cards scheduled for a certain date")
    _ <- putStrLn("4 - Add a new card")
    selectionStr <- readLn
    selection <- {
      val retry: IO[Int] = putStrLn("Wrong input!") >> selectAction
      Try(selectionStr.toInt) match {
        case Failure(_) => retry
        case Success(selectionInt) =>
          if(1 to 4 contains selectionInt) IO.pure(selectionInt)
          else retry
      }
    }
  } yield selection

  def addCard(client: Http4sClient[IO]): IO[Unit] = for {
    _ <- putStrLn("Enter question:")
    question <- readLn
    _ <-
    if(question.length > 250) putStrLn("Wrong question length!") >> addCard(client)
    else {
      for {
        _ <- putStrLn("Enter answer:")
        answer <- readLn
        _ <-
        if(answer.length > 250) putStrLn("Wrong answer length!") >> addCard(client)
        else {
          for {
            today <- IO(LocalDate.now())
            qaObject = QuestionAnswer(question, answer, today)
            cardId <- client.expect[Int](Method.POST(qaObject, uri / "addCard"))
            _ <- putStrLn(s"Added card ID $cardId")
            _ <- main(client)
          } yield ()
        }
      } yield ()
    }
  } yield ()

  def main(client: Http4sClient[IO]): IO[Unit] = for {
    selection <- selectAction
    _ <-
    if(selection < 4) putStrLn(s"You selected $selection")
    else addCard(client)
  } yield ()

  override def run(args: List[String]): IO[ExitCode] =
    BlazeClientBuilder[IO](ExecutionContext.global).resource.use { client =>
      main(client)
    }.as(ExitCode.Success)
}
