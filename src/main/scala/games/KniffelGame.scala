package games

import java.io.IOException

import zio._
import zio.console._
import zio.random._

import scalaz._
import Scalaz._

import scala.util.Try
import State._
import Score._
import Die._

/**
 * Kniffel game
 * 1. choose number of players
 * 2. roll dice
 * 3. choose dice to keep
 * 4. either assign to available hand or re-roll
 * 5. if chose to re-roll repeat step 2-4
 * 6. if chose to re-roll repoeat step 2 and then assign hand
 * 7. next player turn until both players have filled all hands
 **/
object KniffelGame extends App {

  val kniffelGame: ZIO[Console with Random, IOException, Unit] =
    for {
      _               <- putStrLn("Functional Kniffel")
      numberOfPlayers <- getNumberPlayers
      _               <- putStrLn(s"numberOfPlayers $numberOfPlayers")
      playerNames     <- getPlayerNames(numberOfPlayers)
      state            = State(playerNames.map(name => PlayerState.empty(name)))
      finalState      <- gameLoop(state)
      _               <- renderState(finalState)
    } yield ()

  override def run(args: List[String]) =
    kniffelGame.fold(_ => 1, _ => 0)

  private def gameLoop(state: State): ZIO[Console with Random, IOException, State] =
    for {
      _              <- renderState(state)
      currentPlayer   = state.players.head
      newPlayerState <- rollLoop(currentPlayer, state)
      newState        = advancePlayer(updateState(newPlayerState, state))
      finished        = newState.players.flatMap(_.unfilledHands).isEmpty
      finalState     <- if (finished) ZIO.succeed(newState) else gameLoop(newState)
    } yield (finalState)

  private def parseRetainString(retain: String): Reader[FiveDice, Option[List[Die]]] =
    Reader { roll =>
      if (retain.isEmpty()) Some(Nil)
      else {
        val rollList: List[Die] = roll.value

        val parseString: String => Option[List[Die]] =
          _.split(",").toList
            .map(toDie)
            .sequence

        val rollContainsRetained: List[Die] => Option[List[Die]] =
          retain => {
            val dieCounts: List[Die] => Map[Die, Int] = _.groupBy(identity).mapValues(_.length)
            val retVals                               = dieCounts(retain)
            val rollVals                              = dieCounts(rollList)
            retVals.keys
              .map(key => retVals.get(key) <= rollVals.get(key))
              .forall(_ == true)
              .option(retain)
          }

        val getRetained: String => Option[List[Die]] =
          Kleisli(rollContainsRetained) <=< Kleisli(parseString)

        getRetained(retain)
      }
    }

  private def parseAssignString(
      assign: String,
      roll: FiveDice,
      playerState: PlayerState
  ): Option[PlayerState] = {
    val outcomeNames = Map(
      "Ones"         -> Ones,
      "Twos"         -> Twos,
      "Threes"       -> Threes,
      "Fours"        -> Fours,
      "Fives"        -> Fives,
      "Sixes"        -> Sixes,
      "ThreeOfKind"  -> ThreeOfKind,
      "FourOfKind"   -> FourOfKind,
      "FullHouse"    -> FullHouse,
      "Kniffel"      -> Kniffel,
      "FourStraight" -> FourStraight,
      "FiveStraight" -> FiveStraight,
      "Chance"       -> Chance
    )

    for {
      outcome <- outcomeNames.get(assign)
      _       <- playerState.unfilledHands.find(_ == outcome)
    } yield (playerState.copy(
      unfilledHands = playerState.unfilledHands.filterNot(_ == outcome),
      filledHands = playerState.filledHands :+ Assignment(outcome, roll)
    ))

  }

  private val rollNDice: Int => ZIO[Random, Nothing, List[Die]] =
    n => ZIO.traverse(List.fill(n)("foo"))(_ => rollDie)

  private val roll5Dice = rollNDice(5).map(l => FiveDice(l(0), l(1), l(2), l(3), l(4)))

  private def newRandomInt(i: Int) = random.nextInt(i)

  private def rollDie: ZIO[Random, Nothing, Die] =
    newRandomInt(6)
      .map(_ + 1)
      .map(_ match {
        case 1 => One
        case 2 => Two
        case 3 => Three
        case 4 => Four
        case 5 => Five
        case 6 => Six
      })

  private def renderState(state: State): ZIO[Console, IOException, Unit] = {
    def renderPlayerState(playerState: PlayerState): ZIO[Console, IOException, Unit] =
      for {
        _         <- putStrLn("")
        _         <- putStrLn(playerState.name)
        _         <- putStrLn("filled hands")
        _         <- ZIO.traverse(playerState.filledHands.sorted)(fh =>
                       for {
                         _ <-
                           putStrLn(
                             s"Hand: ${fh.outcome} ${" " * (15 - fh.outcome.toString.length)}|   Score: ${assignmentScore(fh)} "
                           )
                       } yield ()
                     )
        topHalfSum = playerState.filledHands.filter(a => isTopHalf(a.outcome)).map(assignmentScore _).sum
        bonus      = if (topHalfSum >= 63) 50 else 0
        _         <- putStrLn(s"bonus: $bonus")
        _         <- putStrLn(s"totalScore: ${playerState.filledHands.map(assignmentScore _).sum + bonus}")
      } yield ()

    for {
      _ <- putStrLn("*" * 10)
      _ <- ZIO.traverse(state.players)(player => putStrLn("") *> renderPlayerState(player) *> putStrLn("-" * 10))
    } yield ()

  }

  private val getNumberPlayers: ZIO[Console, IOException, Int] =
    for {
      ans        <- putStrLn("How many players?") *> getStrLn
      answer      = Try(ans.toInt)
      numPlayers <- answer match {
                      case scala.util.Success(ans) =>
                        if (ans > 0 && ans <= 10) ZIO.succeed(ans)
                        else putStrLn("number must be between 1 and 10") *> getNumberPlayers
                      case scala.util.Failure(_)   =>
                        putStrLn("must enter a valid integer between 1 and 10") *> getNumberPlayers
                    }
    } yield (numPlayers)

  private def getName(n: Int): ZIO[Console, IOException, String] =
    putStrLn(s"player $n: what is your name?") *> getStrLn

  private def getPlayerNames(numberOfPlayers: Int): ZIO[Console, IOException, List[String]] =
    ZIO.traverse((1 to numberOfPlayers).toList)(getName(_))

  private def getAssignment(
      roll: FiveDice,
      currentPlayer: PlayerState,
      state: PlayerState
  ): ZIO[Console, IOException, PlayerState] =
    for {
      _            <- putStrLn("please assign your roll to a hand")
      assignString <- putStrLn(s"${currentPlayer.unfilledHands.toString()}") *> getStrLn
      maybeState   <- parseAssignString(assignString, roll, state) match {
                        case None        =>
                          putStrLn("miscellanious error assigning roll") *> getAssignment(
                            roll,
                            currentPlayer,
                            state
                          )
                        case Some(state) => ZIO.succeed(state)
                      }

    } yield (maybeState)

  private def getRetained(
      roll: FiveDice,
      turnsTaken: Int,
      currentPlayer: PlayerState
  ): ZIO[Console with Random, IOException, FiveDice] = {
    if (turnsTaken >= 2) ZIO.succeed(roll)
    else {
      for {
        retainStr <- putStrLn(s"your roll is $roll") *>
                       putStrLn(s"which dice would you like to keep? ") *>
                       getStrLn
        nextRoll  <- parseRetainString(retainStr).run(roll) match {
                       case None               =>
                         putStrLn("miscellanious error try again with retaining dice") *>
                           getRetained(roll, turnsTaken, currentPlayer)
                       case Some(retainedList) =>
                         val newDice = rollNDice(5 - retainedList.length)
                         val diceSet = newDice map (_ ++ retainedList)
                         diceSet.flatMap(d =>
                           getRetained(FiveDice(d(0), d(1), d(2), d(3), d(4)), turnsTaken + 1, currentPlayer)
                         )
                     }
      } yield (nextRoll)
    }
  }

  private def rollLoop(
      currentPlayer: PlayerState,
      state: State
  ): ZIO[Console with Random, IOException, PlayerState] =
    for {
      _        <- putStrLn(s"current player is ${currentPlayer.name}")
      _        <- putStrLn(s"     unfillled Hands: ${currentPlayer.unfilledHands.sorted}")
      roll     <- roll5Dice
      retained <- getRetained(roll, 0, currentPlayer) //TODO this is actually the full roll, not just retained
      _        <- putStrLn(s"your roll is $retained")
      state    <- getAssignment(
                    retained,
                    currentPlayer,
                    state.players.find(_.name == currentPlayer.name).get
                  )

    } yield (state)

}
