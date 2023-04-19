package com.tictactoe

import homework.Pieces.Piece
import homework.Players.Player

import java.io.IOException

package object homework {

  trait DomainErrors                    extends IOException
  case class NotEmptyIndex(index: Int)  extends DomainErrors
  case class IndexOutOfSize(index: Int) extends DomainErrors
  case object WrongInputFormat          extends DomainErrors

  object Players extends Enumeration {
    type Player = Value

    val Human, Ai = Value
  }

  object Pieces extends Enumeration {
    type Piece = Value

    val X, O, e = Value
  }

  case class PlayerState(`type`: Player, piece: Piece)

  sealed trait GameState
  case class Ongoing(next: Player)                      extends GameState
  case class Finished(current: Player, winner: Boolean) extends GameState

}
