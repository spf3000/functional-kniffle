package games

sealed trait Die { val value: Int; override def toString() = this.value.toString }
case object One   extends Die { val value: Int = 1 }
case object Two   extends Die { val value: Int = 2 }
case object Three extends Die { val value: Int = 3 }
case object Four  extends Die { val value: Int = 4 }
case object Five  extends Die { val value: Int = 5 }
case object Six   extends Die { val value: Int = 6 }

object Die {

  def toDie(s: String): Option[Die] =
    s match {
      case "1" => Some(One)
      case "2" => Some(Two)
      case "3" => Some(Three)
      case "4" => Some(Four)
      case "5" => Some(Five)
      case "6" => Some(Six)
      case _   => None
    }

}
