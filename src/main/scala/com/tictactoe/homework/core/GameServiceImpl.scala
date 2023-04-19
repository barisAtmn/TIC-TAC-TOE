package com.tictactoe.homework.core

import Board.Board
import com.tictactoe.homework.{Finished, GameState, IndexOutOfSize, NotEmptyIndex, Ongoing, PlayerState, WrongInputFormat}
import com.tictactoe.homework.utils.Validation.Validation
import interfaces.GameService
import zio.{IO, UIO, ZIO, ZLayer}

import java.io.IOException

/**
  *  check rows : get array > grouped by dimension > check if all is equal by filtering empty indexes
  *  check columns: get array > grouped by dimension > transpose > check if all is equal by filtering empty indexes
  *  check diagonals: get array > grouped by dimension > zipWithIndex > map with index to get diagonal > check if all is equal by filtering empty indexes
  *                   and
  *                   get array > grouped by dimension > zipWithIndex > map with (dimension - 1- index) to get diagonal > check if all is equal by filtering empty indexes
 **/
case class GameServiceImpl(validationService: Validation, boardService: Board) extends GameService {

  import zio.Console._
  import boardService._
  import validationService._
  import com.tictactoe.homework.Pieces._
  import com.tictactoe.homework.Players._

  override val startGame = (for {
    _           <- print(" \n Enter position = ")
    index       <- readLine
    _           <- validate(index)
    _           <- if (board(index.toInt) == e) ZIO.succeed() else printBoard() *> ZIO.fail(NotEmptyIndex(index.toInt))
    humanResult <- setHuman(index.toInt) *> printLine("\n *** Human Play ***") *> printBoard() *> isWon(human)
    control <- humanResult match {
      case Finished(_, true)  => printLine(s"\n Human won!") *> ZIO.succeed(true)
      case Finished(_, false) => printLine(s"\n Game is tight!") *> ZIO.succeed(true)
      case _                  => ZIO.succeed(false)
    }
    aiResult <- if (!control) setAi() *> printLine("\n *** Ai Play ***") *> printBoard() *> isWon(ai) else ZIO.succeed()
    control2 <- aiResult match {
      case Finished(_, true)  => printLine(s"\n AI won!") *> ZIO.succeed(true)
      case Finished(_, false) => printLine(s"\n Game is tight!") *> ZIO.succeed(true)
      case _                  => ZIO.succeed(false)
    }
  } yield control || control2)
    .catchAll {
      case value @ NotEmptyIndex(_)  => printLine(s"Index number: ${value.index} is not empty").as(false)
      case value @ IndexOutOfSize(_) => printLine(s"Index number: ${value.index} is not in board limit").as(false)
      case WrongInputFormat          => printLine("Wrong input format, please give number within board limits").as(false)
    }
    .repeatUntil(_ == true)

  override def setAi(): IO[_ >: IOException, Unit] = ZIO.succeed {
    board(board.indexOf(e)) = X
  }

  override def setHuman(position: Int): IO[_ >: IOException, Unit] =
    for {
      _ <- if (board(position) == e) ZIO.succeed(board(position) = O) else ZIO.fail(NotEmptyIndex(position))
    } yield ()

  override val isBoardFull: UIO[Boolean] = ZIO.succeed(!board.exists(_ == e))

  override def isWon(playerState: PlayerState): IO[_ >: IOException, GameState] =
    for {
      rowState        <- checkRowsForPiece(playerState).fork
      columnState     <- checkColumns(playerState).fork
      diagonalState   <- checkDiagonal(playerState).fork
      rowControl      <- rowState.join
      columnControl   <- columnState.join
      diagonalControl <- diagonalState.join
      indexControl    <- isBoardFull
      result = {
        if ((columnControl.isInstanceOf[Finished] && columnControl.asInstanceOf[Finished].winner)
            ||
            (rowControl.isInstanceOf[Finished] && rowControl.asInstanceOf[Finished].winner)
            ||
            (diagonalControl.isInstanceOf[Finished] && diagonalControl.asInstanceOf[Finished].winner)) Finished(playerState.`type`, true)
        else if (indexControl == true) Finished(playerState.`type`, false)
        else rowControl
      }
    } yield result

  override def printBoard(): UIO[Unit] =
    for {
      effect <- ZIO.succeed(
        board
          .grouped(boardDimension)
          .map(_.mkString("|"))
          .mkString("_" * dimension * 2 + "\n", "\n" + "_" * dimension * 2 + "\n", ""))
      _ <- printLine(effect).ignore
      _ <- printLine("_" * dimension * 2).ignore
    } yield ()

  def checkRowsForPiece(playerState: PlayerState): IO[_ >: IOException, GameState] =
    for {
      control <- ZIO.succeed(
        board
          .grouped(boardDimension)
          .exists(innerArr =>
            innerArr.forall(_ != e)
              && (innerArr.count(_ == playerState.piece) == boardDimension)))
      result <- if (control == true) ZIO.succeed(Finished(playerState.`type`, true))
      else ZIO.succeed(Ongoing(if (playerState.`type` == Human) Ai else Human))
    } yield result

  def checkColumns(playerState: PlayerState): IO[_ >: IOException, GameState] =
    for {
      control <- ZIO.succeed(
        board
          .grouped(boardDimension)
          .toList
          .transpose
          .exists(
            innerArr =>
              innerArr.forall(_ != e)
                && (
                  innerArr.count(_ == playerState.piece) == boardDimension
              )))
      result <- if (control == true) ZIO.succeed(Finished(playerState.`type`, true))
      else ZIO.succeed(Ongoing(if (playerState.`type` == Human) Ai else Human))
    } yield result

  def checkDiagonal(playerState: PlayerState): IO[_ >: IOException, GameState] =
    for {
      firstDiagonal  <- ZIO.succeed(board.grouped(boardDimension).zipWithIndex.map(tuple => tuple._1(tuple._2)).toList)
      result         <- ZIO.succeed { (firstDiagonal.distinct.size == 1) && (firstDiagonal.count(_.equals(e)) == 0) }
      secondDiagonal <- ZIO.succeed(board.grouped(boardDimension).zipWithIndex.map(tuple => tuple._1(boardDimension - 1 - tuple._2)).toList)
      result2        <- ZIO.succeed { (secondDiagonal.distinct.size == 1) && (secondDiagonal.count(_.equals(e)) == 0) }
      combined <- if ((result || result2) == true) ZIO.succeed(Finished(playerState.`type`, true))
      else ZIO.succeed(Ongoing(if (playerState.`type` == Human) Ai else Human))
    } yield combined

}

object GameServiceImpl {

  lazy val live: ZLayer[Validation with Board, Nothing, GameServiceImpl] = ZLayer.fromFunction(GameServiceImpl(_, _))

}
