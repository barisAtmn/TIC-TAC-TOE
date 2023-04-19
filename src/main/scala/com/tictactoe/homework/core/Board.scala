package com.tictactoe.homework.core

import com.tictactoe.homework.Pieces.{O, Piece, X, e}
import com.tictactoe.homework.PlayerState
import com.tictactoe.homework.Players.{Human, Ai}
import zio.ZLayer

object Board {

  case class Board(board: Array[Piece], boardDimension: Int, human: PlayerState, ai: PlayerState)

  val live: Int => ZLayer[Any, Nothing, Board] = dimension =>
    ZLayer.succeed(
      Board(Array.fill(scala.math.pow(dimension, 2).toInt) { e }, dimension, PlayerState(Human, O), PlayerState(Ai, X))
  )

}
