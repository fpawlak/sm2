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
    _ <- putStrLn("\nSelect action:")
    _ <- putStrLn("1 - List all cards by date")
    _ <- putStrLn("2 - Review cards scheduled for today")
    _ <- putStrLn("3 - Review cards scheduled for a certain date")
    _ <- putStrLn("4 - Add a new card")
    _ <- putStrLn("5 - Delete a card")
    _ <- putStrLn("6 - Modify a card")
    _ <- putStrLn("7 - Display a single card")
    selectionStr <- readLn
    selection <- {
      val retry: IO[Int] = putStrLn("\nWrong input!") >> selectAction
      Try(selectionStr.toInt) match {
        case Failure(_) => retry
        case Success(selectionInt) =>
          if(1 to 7 contains selectionInt) IO.pure(selectionInt)
          else retry
      }
    }
  } yield selection

  // listing all cards

  def printOnCertainDate(input: (LocalDate, List[Card])): IO[Unit] = {
    val (date, cards) = input
    for {
      _ <- putStrLn(s"\n$date:")
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
    _ <- putStrLn("\nEnter a date in the format yyyy-mm-dd:")
    inputStr <- readLn
    result <- Try(LocalDate.parse(inputStr)) match {
      case Success(date) => IO.pure(date)
      case Failure(_) => putStrLn("\nWrong date format!") >> getDate
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
    _ <- putStrLn(s"\n${card.id}. ${card.question}")
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
    _ <- putStrLn("\nEnter question:")
    question <- readLn
    res <- {
      if(question.length > 250)
        putStrLn("Wrong question length!") >> getQuestion
      else
        IO.pure(question)
    }
  } yield res

  def getAnswer: IO[String] = for {
    _ <- putStrLn("\nEnter answer:")
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
    _ <- putStrLn(s"\nAdded card ID $cardId")
    _ <- main(client)
  } yield ()

  // function used in deleting, modifying and displaying a card

  def getCardId: IO[Int] = for {
    _ <- putStrLn("\nEnter card ID:")
    idStr <- readLn
    res <- Try(idStr.toInt) match {
      case Success(id) => IO.pure(id)
      case Failure(_) => putStrLn("\nWrong input!") >> getCardId
    }
  } yield res

  // deleting cards

  def deleteCard(client: Http4sClient[IO]): IO[Unit] = for {
    cardId <- getCardId
    request <- Method.DELETE(uri / "card" / cardId.toString)
    _ <- client.run(request).use { response =>
      if(response.status.code == 200) putStrLn("\nCard deleted!")
      else if(response.status.code == 404) putStrLn("\nNo such card!")
      else putStrLn(s"\nError, HTTP status ${response.status.code}!")
    }
    _ <- main(client)
  } yield ()

  // modifying a card

  def modifyCard(client: Http4sClient[IO]): IO[Unit] = for {
    cardId <- getCardId
    request <- Method.GET(uri / "card" / cardId.toString)
    status <- client.run(request).use { response =>
      IO.pure(response.status.code)
    }
    _ <- {
      if(status == 200) {
        for {
          newQuestion <- getQuestion
          newAnswer <- getAnswer
          today <- IO(LocalDate.now())
          qaObject = QuestionAnswer(newQuestion, newAnswer, today)
          _ <- client.expect[String](Method.PUT(qaObject, uri / "card" / cardId.toString))
          _ <- putStrLn("\nCard modified!")
          _ <- main(client)
        } yield ()
      }
      else if(status == 404) putStrLn("\nNo such card!") >> main(client)
      else putStrLn(s"\nError, HTTP status $status when fetching the card!") >> main(client)
    }
  } yield ()

  // displaying a card

  def displayCard(client: Http4sClient[IO]): IO[Unit] = for {
    cardId <- getCardId
    request <- Method.GET(uri / "card" / cardId.toString)
    _ <- client.run(request).use { response =>
      if(response.status.code == 200) {
        for {
          card <- response.as[Card]
          _ <- putStrLn(s"\nQuestion: ${card.question}")
          _ <- putStrLn(s"Answer: ${card.answer}")
          _ <- main(client)
        } yield ()
      }
      else if(response.status.code == 404) putStrLn("\nNo such card!") >> main(client)
      else putStrLn(s"\nError, HTTP status ${response.status.code} when fetching the card!") >> main(client)
    }
  } yield ()

  def main(client: Http4sClient[IO]): IO[Unit] = for {
    selection <- selectAction
    _ <- {
      if(selection == 1) listAllCards(client)
      else if(selection == 2) fetchAndGradeCardsToday(client)
      else if(selection == 3) fetchAndGradeCards(client)
      else if(selection == 4) addCard(client)
      else if(selection == 5) deleteCard(client)
      else if(selection == 6) modifyCard(client)
      else if(selection == 7) displayCard(client)
      else putStrLn("Internal error!")
    }
  } yield ()

  override def run(args: List[String]): IO[ExitCode] =
    BlazeClientBuilder[IO](ExecutionContext.global).resource.use { client =>
      main(client)
    }.as(ExitCode.Success)
}
