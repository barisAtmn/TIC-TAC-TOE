package com.tictactoe.homework.core.interfaces

import com.tictactoe.homework.{GameState, PlayerState}
import zio.{IO, UIO}

import java.io.IOException

trait GameService {

  def setAi(): IO[_ >: IOException, Unit]

  def setHuman(position: Int): IO[_ >: IOException, Unit]

  def isWon(playerState: PlayerState): IO[_ >: IOException, GameState]

  def printBoard(): UIO[Unit]

  val startGame: IO[_ >: IOException, Boolean]

  val isBoardFull: IO[_ >: IOException, Boolean]

}
