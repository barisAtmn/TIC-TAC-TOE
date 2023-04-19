package com.tictactoe.homework

import utils.Validation
import core.{ Board, GameServiceImpl }
import core.interfaces.GameService
import zio._

import java.io.IOException

object TicTacToe extends ZIOAppDefault {

  override def run: ZIO[ZIOAppArgs with Scope, Any, Any] = {
    def program(api: GameService): ZIO[Any, IOException, Unit] =
      (for {
        _    <- Console.print("* Tic Tac Toe...")
        _    <- Console.printLine("\n   . Human => 0 , AI => X, empty index => e")
        game <- api.startGame
      } yield game).catchAll(_ => Console.printLine("Unhandled case...")).as()

    for {
      args      <- ZIOAppArgs.getArgs
      boardSize <- if (args.isEmpty) ZIO.succeed(3) else ZIO.succeed(args.head.toInt)
      _ <- ZLayer
        .make[GameService](
          GameServiceImpl.live,
          Board.live(boardSize),
          Validation.live(boardSize)
        )
        .build
        .map(_.get[GameService])
        .flatMap(program)
    } yield ()
  }

}
