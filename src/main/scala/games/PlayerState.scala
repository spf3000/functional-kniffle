package games


  case class PlayerState(name: String, unfilledHands: List[Outcome], filledHands: List[Assignment])
  object PlayerState {

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
    Kniffel,
    FourStraight,
    FiveStraight,
    Chance
  )


    def empty(name: String): PlayerState = new PlayerState(name, emptyHand, List())
  }

