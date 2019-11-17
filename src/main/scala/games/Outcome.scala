package games

import Score._

  abstract class Outcome(val score: FiveDice => Int, val order: Int) extends Ordered[Outcome] {
    def compare(that: Outcome) = this.order.compare(that.order)
  }
  abstract class TopHalfOutcome(score: FiveDice => Int, order: Int) extends Outcome(score, order)
  case object Ones         extends TopHalfOutcome(sumDieMatches(One), 0)
  case object Twos         extends TopHalfOutcome(sumDieMatches(Two), 1)
  case object Threes       extends TopHalfOutcome(sumDieMatches(Three), 2)
  case object Fours        extends TopHalfOutcome(sumDieMatches(Four), 3)
  case object Fives        extends TopHalfOutcome(sumDieMatches(Five), 4)
  case object Sixes        extends TopHalfOutcome(sumDieMatches(Six), 5)
  case object ThreeOfKind  extends Outcome(ofKindScore(3), 6)
  case object FourOfKind   extends Outcome(ofKindScore(4), 7)
  case object FullHouse    extends Outcome(fullHouseScore, 8)
  case object Kniffel      extends Outcome(kniffleScore, 9)
  case object FourStraight extends Outcome(straightScore(4, 25), 10)
  case object FiveStraight extends Outcome(straightScore(5, 40), 11)
  case object Chance       extends Outcome(sumMatchingDice(_ => true), 12)


  object Outcome {
  def isTopHalf(o: Outcome): Boolean = o match {
    case Ones   => true
    case Twos   => true
    case Threes => true
    case Fours  => true
    case Fives  => true
    case Sixes  => true
    case _      => false
  }
  }
