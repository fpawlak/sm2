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
import Domain._

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

  // listing all cards

  def printOnCertainDate(input: (LocalDate, List[Card])): IO[Unit] = {
    val (date, cards) = input
    for {
      _ <- putStrLn(s"$date:")
      _ <- putStrLn(cards.map(_.id).mkString(", "))
    } yield ()
  }

  def listAllCards(client: Http4sClient[IO]): IO[Unit] = {
    implicit val localDateOrdering: Ordering[LocalDate] = Ordering.by(_.toEpochDay)
    for {
      cardList <- client.expect[List[Card]](uri / "cards")
      byDateMap = cardList.groupBy(_.scheduledFor)
      byDateList = byDateMap.toList
      byDateListSorted = byDateList.sortBy(_._1)
      _ <- byDateListSorted.traverse(printOnCertainDate)
      _ <- main(client)
    } yield ()
  }

  // fetching and grading cards scheduled for a certain day

  def getDate: IO[LocalDate] = for {
    _ <- putStrLn("Enter a date in the format yyyy-mm-dd:")
    inputStr <- readLn
    result <- Try(LocalDate.parse(inputStr)) match {
      case Success(date) => IO.pure(date)
      case Failure(_) => putStrLn("Wrong date format!") >> getDate
    }
  } yield result

  def getGrade: IO[Int] = for {
    _ <- putStrLn("Enter a grade (0-5):")
    inputStr <- readLn
    result <- Try(inputStr.toInt) match {
      case Success(number) =>
        if(0 to 5 contains number) IO.pure(number)
        else putStrLn("Wrong input!") >> getGrade
      case Failure(_) => putStrLn("Wrong input!") >> getGrade
    }
  } yield result

  def gradeCard(client: Http4sClient[IO], card: Card, sendGrade: Boolean): IO[Option[Card]] = for {
    _ <- putStrLn(s"${card.id}. ${card.question}")
    _ <- readLn
    _ <- putStrLn(card.answer)
    grade <- getGrade
    _ <- IO.whenA(sendGrade)(client.expect[String](Method.POST(uri / "grade" / card.id.toString / grade.toString)).void)
    result = if(grade < 4) Some(card) else None
  } yield result

  def gradeCards(client: Http4sClient[IO], cards: List[Card], sendGrade: Boolean): IO[Unit] = {
    if(cards.isEmpty) main(client)
    else {
      for {
        listOptCards <- cards.traverse(gradeCard(client, _, sendGrade))
        newCardList = listOptCards.collect { case Some(x) => x }
        res <- gradeCards(client, newCardList, false)
      } yield res
    }
  }

  // fetches and grades cards scheduled for today and before
  def fetchAndGradeCardsToday(client: Http4sClient[IO]): IO[Unit] = for {
    date <- IO(LocalDate.now())
    _ <- client.expect[String](Method.PUT(uri / "updateDates" / date.toString))
    cards <- client.expect[List[Card]](uri / "cards" / date.toString)
    _ <- gradeCards(client, cards, true)
    _ <- main(client)
  } yield ()

  // fetches and grades cards scheduled for a certain date
  def fetchAndGradeCards(client: Http4sClient[IO]): IO[Unit] = for {
    date <- getDate
    cards <- client.expect[List[Card]](uri / "cards" / date.toString)
    _ <- gradeCards(client, cards, true)
    _ <- main(client)
  } yield ()

  // adding new cards

  def getQuestion: IO[String] = for {
    _ <- putStrLn("Enter question:")
    question <- readLn
    res <- {
      if(question.length > 250)
        putStrLn("Wrong question length!") >> getQuestion
      else
        IO.pure(question)
    }
  } yield res

  def getAnswer: IO[String] = for {
    _ <- putStrLn("Enter answer:")
    answer <- readLn
    res <- {
      if(answer.length > 250)
        putStrLn("Wrong answer length!") >> getAnswer
      else
        IO.pure(answer)
    }
  } yield res

  def addCard(client: Http4sClient[IO]): IO[Unit] = for {
    question <- getQuestion
    answer <- getAnswer
    today <- IO(LocalDate.now())
    qaObject = QuestionAnswer(question, answer, today)
    cardId <- client.expect[Int](Method.POST(qaObject, uri / "addCard"))
    _ <- putStrLn(s"Added card ID $cardId")
    _ <- main(client)
  } yield ()

  def main(client: Http4sClient[IO]): IO[Unit] = for {
    selection <- selectAction
    _ <- {
      if(selection == 1) listAllCards(client)
      else if(selection == 2) fetchAndGradeCardsToday(client)
      else if(selection == 3) fetchAndGradeCards(client)
      else if(selection == 4) addCard(client)
      else putStrLn("Internal error!")
    }
  } yield ()

  override def run(args: List[String]): IO[ExitCode] =
    BlazeClientBuilder[IO](ExecutionContext.global).resource.use { client =>
      main(client)
    }.as(ExitCode.Success)
}
