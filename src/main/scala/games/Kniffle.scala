package games

import java.lang.NumberFormatException
import java.io.IOException

import zio._
import zio.console._
import zio.random._

import scalaz._
import Scalaz._

import scala.language.higherKinds
import scala.util.Try

/**
  * Kniffle game
  * 1. choose number of players
  * 2. roll dice
  * 3. choose dice to keep
  * 4. either assign to available hand or re-roll
  * 5. if chose to re-roll repeat step 2-4
  * 6. if chose to re-roll repoeat step 2 and then assign hand
  * 7. next player turn until both players have filled all hands
 **/
object Kniffle extends App {

  sealed trait Die { val value: Int }
  case object One   extends Die { val value: Int = 1 }
  case object Two   extends Die { val value: Int = 2 }
  case object Three extends Die { val value: Int = 3 }
  case object Four  extends Die { val value: Int = 4 }
  case object Five  extends Die { val value: Int = 5 }
  case object Six   extends Die { val value: Int = 6 }

  trait FiveDice {
    val value: List[Die]
    override def toString(): String = value.toString
  }
  object FiveDice {
    def apply(d1: Die, d2: Die, d3: Die, d4: Die, d5: Die) = new FiveDice {
      val value = List(d1, d2, d3, d4, d5)
    }
  }

  def countMatchingDice(f: Die => Boolean): FiveDice => Int = _.value.count(f(_))

  def countDieMatches(d: Die): FiveDice => Int = countMatchingDice(_ == d)

  val groupedDiceLengths: FiveDice => List[Int] =
    _.value
      .groupBy(identity)
      .values
      .map(_.length)
      .toList

  def containsGreaterThanN(i: Int): List[Int] => Boolean = _.exists(_ >= i)

  def containsNOfKind(n: Int): FiveDice => Boolean =
    groupedDiceLengths andThen containsGreaterThanN(n)

  def containsMandNOfKind(m: Int, n: Int): FiveDice => Boolean = fd => {
    val greater           = m.max(n)
    val lesser            = m.min(n)
    val groupedDice       = groupedDiceLengths(fd)
    val sortedGroupedDice = groupedDice.sorted
    val gdMax             = sortedGroupedDice.last
    val gdSecondMax       = sortedGroupedDice.init.last
    gdMax >= greater && gdSecondMax >= lesser
  }

  def sumIfPredicate(cond: FiveDice => Boolean): FiveDice => Int =
    roll => if (cond(roll)) countMatchingDice(_ => true)(roll) else 0

  def ofKindScore(n: Int): FiveDice => Int = sumIfPredicate(containsNOfKind(n))

  def fullHouseScore = sumIfPredicate(containsMandNOfKind(3, 2))

  val allSequential: List[Int] => Boolean = list =>
    list.zip(list.tail).filter(p => p._2 - p._1 <= 1).nonEmpty

  def containsNOfStraight(n: Int): FiveDice => Boolean = { fiveDice =>
    val list = fiveDice.value
    list.map(_.value).sliding(n).exists(allSequential)
  }

  def straightScore(n: Int): FiveDice => Int = (sumIfPredicate _ compose containsNOfStraight)(n)

  abstract class Outcome(val score: FiveDice => Int)
  case object Ones         extends Outcome(countDieMatches(One))
  case object Twos         extends Outcome(countDieMatches(Two))
  case object Threes       extends Outcome(countDieMatches(Three))
  case object Fours        extends Outcome(countDieMatches(Four))
  case object Fives        extends Outcome(countDieMatches(Five))
  case object Sixes        extends Outcome(countDieMatches(Six))
  case object ThreeOfKind  extends Outcome(ofKindScore(3))
  case object FourOfKind   extends Outcome(ofKindScore(4))
  case object FullHouse    extends Outcome(fullHouseScore)
  case object FiveOfKind   extends Outcome(ofKindScore(5))
  case object FourStraight extends Outcome(straightScore(4))
  case object FiveStraight extends Outcome(straightScore(5))
  case object Chance       extends Outcome(countMatchingDice(_ => true))

  case class Assignment(outcome: Outcome, roll: FiveDice)

  val emptyHand = List(
    Ones,
    Twos,
    Threes,
    Fours,
    Fives,
    Sixes,
    ThreeOfKind,
    FourOfKind,
    FullHouse,
    FiveOfKind,
    FourStraight,
    FiveStraight,
    Chance
  )

  case class PlayerState(name: String, unfilledHands: List[Outcome], filledHands: List[Assignment])
  object PlayerState {
    def empty(name: String): PlayerState = new PlayerState(name, emptyHand, List())
  }

  case class State(players: List[PlayerState])

  def advancePlayer(state: State): State =
    State(state.players.tail :+ state.players.head)

  def updateState(playerState: PlayerState, state: State): State =
    State(playerState +: state.players.filterNot(_.name == playerState.name))

  def toDie(s: String): Option[Die] = s match {
    case "1" => Some(One)
    case "2" => Some(Two)
    case "3" => Some(Three)
    case "4" => Some(Four)
    case "5" => Some(Five)
    case "6" => Some(Six)
    case _   => None
  }

  def getDie(r: String, roll: List[Die]): Option[List[Die]] =
    for {
      d <- toDie(r)
      c <- rollContains(d, roll)
    } yield (c)

  def rollContains(die: Die, roll: List[Die]): Option[List[Die]] =
    roll
      .contains(die) match {
      case false => None
      case true  => Some(roll.tail)
    }

  //TODO needs to return an Option[FiveDice]
  def parseRetainString(retain: String): Reader[FiveDice, Option[List[Die]]] = Reader { roll =>
    {
      val rollList: List[Die] = roll.value

      val retainedDice: String => Option[List[Die]] =
        _.split(",").toList
          .map(toDie)
          .sequence

      val isValidRetainment: List[Die] => Option[List[Die]] =
        retain => {
          val dieCounts: List[Die] => Map[Die, Int] = _.groupBy(identity).mapValues(_.length)
          val retVals                               = dieCounts(retain)
          val rollVals                              = dieCounts(rollList)
          retVals.keys
            .map(key => retVals.get(key) <= rollVals.get(key))
            .forall(_ == true)
            .option(retain)
//          retVals.keys.forall(x => retVals(x) <= rollVals(x)).option(retain)
        }

      val validNumberRetainments: List[Die] => Option[List[Die]] =
        l => if (l.length <= 5) Some(l) else None

      val getRetained: String => Option[List[Die]] =
        Kleisli(retainedDice) >=> Kleisli(isValidRetainment) >=> Kleisli(validNumberRetainments)

      getRetained(retain)
    }
  }

  private def getAssignment(
      roll: FiveDice,
      currentPlayer: String,
      state: PlayerState
  ): ZIO[Console, IOException, PlayerState] =
    for {
      _            <- putStrLn("please assign your roll to a hand")
      assignString <- putStrLn(state.toString()) *> getStrLn
      maybeState <- parseAssignString(assignString, roll, state) match {
        case None =>
          putStrLn("miscellanious error assigning roll") *> getAssignment(
            roll,
            currentPlayer,
            state
          )
        case Some(state) => ZIO.succeed(state)
      }

    } yield (maybeState)

  def parseAssignString(
      assign: String,
      roll: FiveDice,
      playerState: PlayerState
  ): Option[PlayerState] = {
    val outcomeNames = Map(
      "ones"      -> Ones,
      "twos"      -> Twos,
      "threes"    -> Threes,
      "fours"     -> Fours,
      "fives"     -> Fives,
      "sixes"     -> Sixes,
      "3ofKind"   -> ThreeOfKind,
      "4ofKind"   -> FourOfKind,
      "fullHouse" -> FullHouse,
      "kniffle"   -> FiveOfKind,
      "4straight" -> FourStraight,
      "5straight" -> FiveStraight,
      "chance"    -> Chance
    )

    for {
      outcome <- outcomeNames.get(assign)
      _       <- playerState.unfilledHands.find(_ == outcome)
    } yield (playerState.copy(
      unfilledHands = playerState.unfilledHands.filterNot(_ == outcome),
      filledHands = playerState.filledHands :+ Assignment(outcome, roll)
    ))

  }

  //TODO take intersection with old dice

  private def getRetained(
      roll: FiveDice,
      turnsTaken: Int,
      currentPlayer: String
  ): ZIO[Console with Random, IOException, FiveDice] = {
    if (turnsTaken >= 2) ZIO.succeed(roll)
    else {
      for {
        retainStr <- putStrLn(s"your roll is $roll") *>
          putStrLn(s"which dice would you like to keep? ") *>
          getStrLn
        nextRoll <- parseRetainString(retainStr).run(roll) match {
          case None =>
            putStrLn("miscellanious error try again with retaining dice") *>
              getRetained(roll, turnsTaken, currentPlayer)
          case Some(retainedList) =>
            val newDice = rollNDice(5 - retainedList.length)
            val diceSet = newDice map (_ ++ retainedList)
            diceSet.flatMap(
              d =>
                getRetained(FiveDice(d(0), d(1), d(2), d(3), d(4)), turnsTaken + 1, currentPlayer)
            )
        }
      } yield (nextRoll)
    }
  }

  private def rollLoop(
      currentPlayer: String,
      state: State
  ): ZIO[Console with Random, IOException, PlayerState] =
    for {
      _        <- putStrLn(s"current player is $currentPlayer")
      roll     <- roll5Dice
      retained <- getRetained(roll, 0, currentPlayer) //TODO this is actually the full roll, not just retained
      _        <- putStrLn(s"your roll is $retained")
      state <- getAssignment(
        retained,
        currentPlayer,
        state.players.find(_.name == currentPlayer).get
      )

    } yield (state)

  private def gameLoop(state: State): ZIO[Console with Random, IOException, State] =
    for {
      _ <- putStrLn("your turn")
      _ <- renderState(state)
      currentPlayer = state.players.head
      newPlayerState <- rollLoop(currentPlayer.name, state)
      newState = advancePlayer(updateState(newPlayerState, state))
      _ <- putStrLn(s"exited roll loop - newState $newState")
      //TODO update state
      finished = newState.players.flatMap(_.unfilledHands).isEmpty
      finalState <- if (finished) ZIO.succeed(newState) else gameLoop(newState)
    } yield (finalState)

  def nextInt(max: Int): ZIO[Random, Nothing, Int] =
    nextInt(max)

  private val rollNDice: Int => ZIO[Random, Nothing, List[Die]] =
    n => ZIO.traverse(List.fill(n)("foo"))(_ => rollDie)

  val roll5Dice = rollNDice(5).map(l => FiveDice(l(0), l(1), l(2), l(3), l(4)))

  private def rollDie: ZIO[Random, Nothing, Die] =
    nextInt(5)
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
    putStrLn(s"the state is: ${state.players.map(_.toString).mkString("\n   ")}")
  }

  private val getNumberPlayers: ZIO[Console, IOException, Int] =
    for {
      ans <- putStrLn("How many players?") *> getStrLn
      answer = Try(ans.toInt)
      numPlayers <- answer match {
        case scala.util.Success(ans) =>
          if (ans > 0 && ans <= 10) ZIO.succeed(ans)
          else putStrLn("number must be between 1 and 10") *> getNumberPlayers
        case scala.util.Failure(_) =>
          putStrLn("must enter a valid integer between 1 and 10") *> getNumberPlayers
      }
    } yield (numPlayers)

  private def getName(n: Int): ZIO[Console, IOException, String] =
    putStrLn(s"player $n: what is your name?") *> getStrLn

  private def getPlayerNames(numberOfPlayers: Int): ZIO[Console, IOException, List[String]] =
    ZIO.traverse((1 to numberOfPlayers).toList)(getName(_))

  val kniffleGame: ZIO[Console with Random, IOException, Unit] =
    for {
      _               <- putStrLn("Functional Kniffle")
      numberOfPlayers <- getNumberPlayers
      _               <- putStrLn(s"numberOfPlayers $numberOfPlayers")
      playerNames     <- getPlayerNames(numberOfPlayers)
      state = State(playerNames.map(name => PlayerState.empty(name)))
      _          <- renderState(state)
      finalState <- gameLoop(state)
      _          <- renderState(finalState)
    } yield ()

  override def run(args: List[String]) =
    kniffleGame.fold(_ => 1, _ => 0)

}
