package games

  trait FiveDice {
    val value: List[Die]
    override def toString(): String = value.toString
  }
  object FiveDice {
    def apply(d1: Die, d2: Die, d3: Die, d4: Die, d5: Die) = new FiveDice {
      val value = List(d1, d2, d3, d4, d5)
    }
  }
