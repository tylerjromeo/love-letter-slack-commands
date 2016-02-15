package org.romeo.loveletter.game

import scala.util.Random
import scala.collection.immutable.Stack

import scalaz.State

case class Game(players: Seq[Player],
  deck: Seq[Card] = Deck.cards,
  discard: Stack[Card] = Stack.empty,
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

  /**
   * Remove the top card from the deck and add it to the visible discard pile, then return it
   * Used in 2 player games when 3 cards are exposed at the beginning of each round
   */
  def burnCardVisible: State[Game, Card] = {
    //ok to keep this local because no one else should need it
    def addToVisibleDiscard(card: Card): State[Game, Card] = State[Game, Card] {
      g: Game => (g.copy(visibleDiscard = g.visibleDiscard :+ card), card)
    }
    burnCard.flatMap(addToVisibleDiscard)
  }

//TODO: refactor player functions to take in player name and pull player out of current state
  /**
   * Remove the top card from the deck and add it to a player's hand. Then return that player object
   */
  def drawCard(p: Player): State[Game, Player] = {
    burnCard.flatMap(card => updatePlayer(p.copy(hand = p.hand :+ card)))
  }

  /**
   * replaces the player with the same name as the passed in player in the player list. Returns the new player
   * undefined behavior if the player doesn't match one in the game
   */
  def updatePlayer(p: Player) = State[Game, Player] {
    g: Game => (g.copy(players = g.players.updated(g.players.indexWhere(_.name == p.name), p)), p)
  }

  /**
   * removes a card from the given players hand and puts it in the discard pile. Returns the discard pile
   * undefined behavior if the player doesn't have the given card
   */
  def playerDiscard(p: Player, c: Card): State[Game, Stack[Card]] = {
    for {
      _ <- updatePlayer(p.copy(hand = p.hand.diff(Seq(c))))
      d <- discard(c)
    } yield d
  }

  /**
   * Adds a card to teh discard pile, then returns the discard pile
   */
  def discard(c: Card) = State[Game, Stack[Card]] {
    g: Game => {
      val newDiscard = g.discard.push(c)
      (g.copy(discard = newDiscard), newDiscard)
    }
  }
}

case class Player(name: String,//name must be unique among players in the game
  hand: Seq[Card] = Nil,
  isEliminated: Boolean = false,
  isProtected: Boolean = false,
  score: Int = 0
)
