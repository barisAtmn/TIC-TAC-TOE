package com.tictactoe.homework

import com.tictactoe.homework.Pieces.{O, X}
import com.tictactoe.homework.Players.{Ai, Human}
import com.tictactoe.homework.core.{Board, GameServiceImpl}
import com.tictactoe.homework.utils.Validation
import zio.test.Assertion.{equalTo, fails}
import zio.test.TestAspect.nonFlaky
import zio.test.{Spec, TestEnvironment, assert}
import zio.{Console, ZIO, ZLayer}

object GameServiceSpec extends zio.test.ZIOSpecDefault {

  val layersForDimension3 = ZLayer.succeed(Console.ConsoleLive) ++ Board.live(3) ++ Validation.live(3) >>> GameServiceImpl.live

  def spec: Spec[TestEnvironment, Any] =
    suite("GameService Logic")(
      test("[validationService.isEmpty] -- Empty input test") {
        for {
          api    <- ZIO.service[GameServiceImpl]
          result <- api.validationService.isEmpty("").exit
        } yield assert(result)(fails(equalTo(WrongInputFormat)))
      },
      test("[validationService.isNumber] -- Wrong input test") {
        for {
          api    <- ZIO.service[GameServiceImpl]
          result <- api.validationService.isNumber("asd").exit
        } yield assert(result)(fails(equalTo(WrongInputFormat)))
      },
      test("[isWon] -- check if Ai won") {
        for {
          api    <- ZIO.service[GameServiceImpl]
          _      <- api.setAi() *> api.setAi() *> api.setAi()
          result <- api.isWon(PlayerState(Ai, X))
        } yield assert(result)(equalTo(Finished(Ai, true)))
      },
      test("[isWon] -- check if Human won with row checks") {
        for {
          api    <- ZIO.service[GameServiceImpl]
          _      <- api.setHuman(0) *> api.setHuman(1) *> api.setHuman(2)
          result <- api.isWon(PlayerState(Human, O))
        } yield assert(result)(equalTo(Finished(Human, true)))
      },
      test("[isWon] -- check if Human won with column checks") {
        for {
          api    <- ZIO.service[GameServiceImpl]
          _      <- api.setHuman(0) *> api.setHuman(3) *> api.setHuman(6)
          result <- api.isWon(PlayerState(Human, O))
        } yield assert(result)(equalTo(Finished(Human, true)))
      },
      test("[isWon] -- check if Human won with diagonal checks left-to-right") {
        for {
          api    <- ZIO.service[GameServiceImpl]
          _      <- api.setHuman(0) *> api.setHuman(4) *> api.setHuman(8)
          result <- api.isWon(PlayerState(Human, O))
        } yield assert(result)(equalTo(Finished(Human, true)))
      },
      test("[isWon] -- check if Human won with diagonal checks right-to-left") {
        for {
          api    <- ZIO.service[GameServiceImpl]
          _      <- api.setHuman(2) *> api.setHuman(4) *> api.setHuman(6)
          result <- api.isWon(PlayerState(Human, O))
        } yield assert(result)(equalTo(Finished(Human, true)))
      },
      test("[isWon] -- check if game is tight") {
        for {
          api <- ZIO.service[GameServiceImpl]
          _ <- api.setHuman(1) *> api.setAi() *> api.setHuman(4) *>
            api.setAi() *> api.setHuman(5) *> api.setAi() *> api.setHuman(6) *> api.setAi() *> api.setHuman(8)
          result <- api.isWon(PlayerState(Human, O))
        } yield assert(result)(equalTo(Finished(Human, false)))
      },
    ).provideCustomLayer(
      layersForDimension3
    ) @@ nonFlaky

}
