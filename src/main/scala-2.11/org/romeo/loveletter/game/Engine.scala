package org.romeo.loveletter.game

import scalaz.State

case class Game(val players: Seq[Player]) {
  require(players.length >= 2, "Need at least 2 players")
  require(players.length <= 4, "No more than 4 players")
}

object Game {
  def apply(playerNames: => Seq[String]) = new Game(playerNames.map(Player(_)))
  def currentPlayer = State[Game, Player]{g: Game => (g, g.players.head)}
  //Cycles the list of players so the player in front is last. Returns the new current player
  def endTurn = State[Game, Player] {
    g: Game => (Game(g.players.tail ++ Seq(g.players.head)), g.players(1))
  }
}

case class Player(name: String,
  hand: Seq[Card] = Nil,
  isEliminated: Boolean = false,
  isProtected: Boolean = false,
  score: Int = 0
)
