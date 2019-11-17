package games

  sealed trait Die { val value: Int; override def toString() = this.value.toString }
  case object One   extends Die { val value: Int = 1 }
  case object Two   extends Die { val value: Int = 2 }
  case object Three extends Die { val value: Int = 3 }
  case object Four  extends Die { val value: Int = 4 }
  case object Five  extends Die { val value: Int = 5 }
  case object Six   extends Die { val value: Int = 6 }

