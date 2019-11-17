package games

case class State(players: List[PlayerState])

object  State {

  def advancePlayer(state: State): State =
    State(state.players.tail :+ state.players.head)

  def updateState(playerState: PlayerState, state: State): State =
    State(playerState +: state.players.filterNot(_.name == playerState.name))


}
