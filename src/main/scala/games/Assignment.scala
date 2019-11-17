package games


  case class Assignment(outcome: Outcome, roll: FiveDice) extends Ordered[Assignment] {
    def compare(that: Assignment) = this.outcome.compare(that.outcome)
  }

