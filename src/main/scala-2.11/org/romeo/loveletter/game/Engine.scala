package org.romeo.loveletter.game

class Game(val playerNames: Seq[String]){
  require(playerNames.length >= 2, "Need at least 2 players")
  require(playerNames.length <= 4, "No more than 4 players")
  var players = playerNames.map(Player(_))

  def currentPlayer = players.head
  def endTurn(): Unit = {
    players = players.tail ++ Seq(players.head)
  }
}

case class Player(name: String,
  hand: Seq[Card] = Nil,
  isEliminated: Boolean = false,
  isProtected: Boolean = false,
  score: Int = 0
)
