package games

object Score {

  def sumMatchingDice(f: Die => Boolean): FiveDice => Int = _.value.filter(f(_)).map(_.value).sum

  def sumDieMatches(d: Die): FiveDice => Int = sumMatchingDice(_ == d)

  def fullHouseScore = scoreIfPredicate(containsMandNOfKind(3, 2), _ => 30)

  def kniffleScore = scoreIfPredicate(containsNOfKind(5), _ => 50)

  def isTopHalf(o: Outcome): Boolean = o match {
    case Ones   => true
    case Twos   => true
    case Threes => true
    case Fours  => true
    case Fives  => true
    case Sixes  => true
    case _      => false
  }

  def straightScore(n: Int, s: Int): FiveDice => Int =
    scoreIfPredicate(containsNOfStraight(n), _ => s)

  def assignmentScore(a: Assignment): Int = a.outcome.score(a.roll)

  def ofKindScore(n: Int): FiveDice => Int = sumIfPredicate(containsNOfKind(n))

  private def containsNOfStraight(n: Int): FiveDice => Boolean = { fiveDice =>
    def allSequential(ints: List[Int]): Boolean =
      ints.sorted.zip(ints.tail).forall(p => p._2 - p._1 == 1)

    fiveDice.value.map(_.value).distinct.sorted.sliding(n).exists(allSequential)
  }

  private val groupedDiceLengths: FiveDice => List[Int] =
    _.value
      .groupBy(identity)
      .values
      .map(_.length)
      .toList

  private def containsGreaterThanN(n: Int): List[Int] => Boolean = _.exists(_ >= n)

  private def containsNOfKind(n: Int): FiveDice => Boolean =
    groupedDiceLengths andThen containsGreaterThanN(n)

  private def containsMandNOfKind(m: Int, n: Int): FiveDice => Boolean = fd => {
    val greater           = m.max(n)
    val lesser            = m.min(n)
    val groupedDice       = groupedDiceLengths(fd)
    val sortedGroupedDice = groupedDice.sorted
    val gdMax             = sortedGroupedDice.last
    val gdSecondMax       = sortedGroupedDice.init.last
    gdMax >= greater && gdSecondMax >= lesser
  }

  private def sumIfPredicate(cond: FiveDice => Boolean): FiveDice => Int =
    roll => if (cond(roll)) sumMatchingDice(_ => true)(roll) else 0

  private def scoreIfPredicate(cond: FiveDice => Boolean, scoreFunc: FiveDice => Int): FiveDice => Int =
    roll => if (cond(roll)) scoreFunc(roll) else 0

}
