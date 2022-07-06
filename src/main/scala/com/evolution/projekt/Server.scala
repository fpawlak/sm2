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

object Server extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    IO(println("Hello from the server!")).as(ExitCode.Success)
}
