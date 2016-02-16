package org.romeo.loveletter.game

import scala.util.Random
import scala.collection.immutable.List

import scalaz.State

case class Game(players: Seq[Player],
  deck: Seq[Card] = Deck.cards,
  discard: List[Card] = Nil,
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
   * Starts a new match with the game's players. Throws away the old hand and discard states
   */
  def newMatch = State[Game, Unit] {
    g: Game => {
      (Game(g.players.map(p => Player(name = p.name, score = p.score))), {})
    }
  }

  def startMatch(r: Random) = {
    def burn3VisibleIfTwoPlayer = State[Game, Seq[Card]] {
      g: Game => if(g.players.length == 2) {
        def burn3 = for {
          a <- burnCardVisible
          b <- burnCardVisible
          c <- burnCardVisible
        } yield a ++ b ++ c
        burn3(g)
      } else {
        (g, Nil)
      }
    }
    def dealFirstCards = State[Game, Unit] {
      game: Game => (game.players.foldLeft(game)((g, p) => drawCard(p.name).exec(g)), {})
    }
    for {
      _ <- newMatch
      _ <- shuffle(r)
      _ <- burnCard
      _ <- burn3VisibleIfTwoPlayer
      _ <- dealFirstCards
      p <- currentPlayer
      _ <- drawCard(p.name)
    } yield ()
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
  def burnCardVisible: State[Game, Seq[Card]] = {
    //ok to keep this local because no one else should need it
    def addToVisibleDiscard(card: Card): State[Game, Card] = State[Game, Card] {
      g: Game => (g.copy(visibleDiscard = g.visibleDiscard :+ card), card)
    }
    burnCard.flatMap(addToVisibleDiscard).map(Seq(_))
  }

  /**
   * Gets the player with the given name (ignoring case) from the game state, or none if no player exists
   */
  def getPlayer(playerName: String) = State[Game, Option[Player]] {
    g: Game => (g, g.players.find(_.name.equalsIgnoreCase(playerName)))
  }

  /**
   * Remove the top card from the deck and add it to a player's hand. Then return that player object
   */
  def drawCard(playerName: String): State[Game, Option[Player]] = for {
    c <- burnCard
    player <- getPlayer(playerName)
    player2 <- updatePlayer(player.map(p => p.copy(hand = p.hand :+ c)))
  } yield player2

  /**
   * replaces the player with the same name as the passed in player in the player list. Returns the new player
   * if None is passed in, jus return the same old game state and none
   */
  def updatePlayer(player: Option[Player]) = State[Game, Option[Player]] {
    g: Game => player.map(
      p => (g.copy(players = g.players.updated(g.players.indexWhere(_.name == p.name), p)), Some(p))
    ).getOrElse((g, None:Option[Player]))
  }

  /**
   * removes a card from the given players hand and puts it in the discard pile. Returns the discard pile
   * does not change the state and returns an empty list if an invalid player or card is selected
   */
  def playerDiscard(playerName: String, c: Card): State[Game, List[Card]] = {
    getPlayer(playerName).flatMap(_.flatMap({
        p => if(p.hand.contains(c)) {
          Some(updatePlayer(Some(p.copy(hand = p.hand.diff(Seq(c))))).flatMap(_ => discard(c)))
        } else {
          None
        }
      }).getOrElse(State[Game, List[Card]](g => (g, Nil))) )
  }

  def awardPoint(playerName: String): State[Game, Option[Player]] = {
    getPlayer(playerName).flatMap(player => {
      updatePlayer(player.map(p => p.copy(score = p.score + 1)))
    })
  }

  /**
   * Adds a card to the discard pile, then returns the discard pile
   */
  def discard(c: Card) = State[Game, List[Card]] {
    g: Game => {
      val newDiscard = c :: g.discard
      (g.copy(discard = newDiscard), newDiscard)
    }
  }

  def findWinner = State[Game, Option[Player]] {
    g: Game => {
      val threshold = g.players.length match {
        case 2 => 7
        case 3 => 5
        case 4 => 4
        case _ => throw new AssertionError("Game can only have 2-4 players")
      }
      (g, g.players.find(_.score >= threshold))
    }
  }
}

case class Player(name: String,//name must be unique among players in the game
  hand: Seq[Card] = Nil,
  isEliminated: Boolean = false,
  isProtected: Boolean = false,
  score: Int = 0
)
