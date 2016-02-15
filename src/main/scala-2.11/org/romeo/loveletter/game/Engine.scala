package org.romeo.loveletter.game

import scala.util.Random

import scalaz.State

case class Game(players: Seq[Player],
  deck: Seq[Card] = Deck.cards,
  discard: Seq[Card] = Nil,
  visibleDiscard: Seq[Card] = Nil) {
  require(players.length >= 2, "Need at least 2 players")
  require(players.length <= 4, "No more than 4 players")
}

object Game {
  /**
   * Makes a new game object with the given player names
   */
  def apply(playerNames: => Seq[String]) = new Game(playerNames.map(Player(_)))

  /**
   * Returns the player object for the player who's turn it is. Does not change the game state
   */
  def currentPlayer = State[Game, Player]{g: Game => (g, g.players.head)}

  /**
   * Cycles the list of players so the player in front is last. Returns the new current player
   */
  def endTurn = State[Game, Player] {
    g: Game => (Game(g.players.tail ++ Seq(g.players.head)), g.players(1))
  }

  /**
   * Puts the contents of the deck in random order, and returns the deck
   */
  def shuffle(r: Random):State[Game, Seq[Card]] = State[Game, Seq[Card]] {
    g: Game => {
      val newDeck = r.shuffle(g.deck)
      (g.copy(deck = newDeck), newDeck)
    }
  }

  /**
   * Removes the top card from the deck and returns it. The card will not be put in the discard pile
   */
  def burnCard = State[Game, Card] {
    g: Game => (g.copy(deck = g.deck.tail), g.deck.head)
  }


}

case class Player(name: String,
  hand: Seq[Card] = Nil,
  isEliminated: Boolean = false,
  isProtected: Boolean = false,
  score: Int = 0
)
