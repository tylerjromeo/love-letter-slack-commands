package org.romeo.loveletter.game

case class Game(val players: Seq[Player]) {
  require(players.length >= 2, "Need at least 2 players")
  require(players.length <= 4, "No more than 4 players")
}

object Game {
  def apply(playerNames: => Seq[String]) = new Game(playerNames.map(Player(_)))
  def currentPlayer(g: Game): Player = g.players.head
  def endTurn(g: Game): Game = {
    Game(g.players.tail ++ Seq(g.players.head))
  }
}

case class Player(name: String,
  hand: Seq[Card] = Nil,
  isEliminated: Boolean = false,
  isProtected: Boolean = false,
  score: Int = 0
)
