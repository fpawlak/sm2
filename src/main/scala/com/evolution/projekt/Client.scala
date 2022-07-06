package com.evolution.projekt

import cats._
import cats.data._
import cats.effect._
import cats.implicits._

object Client extends IOApp {

  override def run(args: List[String]): IO[ExitCode] = {
    IO(println("Hello from the client!")).as(ExitCode.Success)
  }
}
