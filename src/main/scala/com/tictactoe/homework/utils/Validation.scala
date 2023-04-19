package com.tictactoe.homework.utils

import com.tictactoe.homework.{IndexOutOfSize, WrongInputFormat}
import zio.{IO, ZIO, ZLayer}

import java.io.IOException

/**
  *  Validation Module for input from command line
**/
object Validation {

  trait Service {
    def validate(input: String): IO[_ >: IOException, Boolean]
    def isNumber(input: String): IO[_ >: IOException, Boolean]
    def indexCheck(input: String): IO[_ >: IOException, Boolean]
    def isEmpty(input: String): IO[_ >: IOException, Boolean]
  }

  case class Validation(dimension: Int) extends Service {
    override def validate(value: String): IO[_ >: IOException, Boolean] =
      for {
        _      <- isEmpty(value)
        _      <- isNumber(value)
        result <- indexCheck(value)
      } yield result

    override def isEmpty(value: String): IO[_ >: IOException, Boolean] =
      if (value == "") ZIO.fail(WrongInputFormat)
      else ZIO.succeed(true)

    override def isNumber(value: String): IO[_ >: IOException, Boolean] =
      if (value forall Character.isDigit) ZIO.succeed(true)
      else ZIO.fail(WrongInputFormat)

    override def indexCheck(value: String): IO[_ >: IOException, Boolean] =
      if (value.toInt >= 0 && value.toInt < scala.math.pow(dimension, 2)) ZIO.succeed(true)
      else ZIO.fail(IndexOutOfSize(value.toInt))

  }

  val live: Int => ZLayer[Any, Nothing, Validation] = dimension => ZLayer.succeed(Validation(dimension))

}
