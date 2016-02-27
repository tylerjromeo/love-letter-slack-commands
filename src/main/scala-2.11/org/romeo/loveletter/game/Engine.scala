package org.romeo.loveletter.game

import scala.util.Random
import scala.collection.immutable.List
import scala.language.implicitConversions

import scalaz.State

case class Player(
  name: String, //name must be unique among players in the game
  hand: Seq[Card] = Nil,
  isEliminated: Boolean = false,
  isProtected: Boolean = false,
  score: Int = 0){
    override def toString: String = s"$name, cards:(${hand.map(_.name).mkString(", ")}), isElminated:$isEliminated, isProtected:$isProtected, score:$score"
  }

object Player {
  implicit def orderingByMaxCard[A <: Player]: Ordering[A] =
    Ordering.by(a => a.hand.max.value)
}

case class Game(
  players: Seq[Player],
  deck: Seq[Card] = Deck.cards,
  burnCard: Seq[Card] = Nil,
  discard: List[Card] = Nil,
  visibleDiscard: Seq[Card] = Nil) {
  require(players.length >= 2, "Need at least 2 players")
  require(players.length <= 4, "No more than 4 players")
}

object Game {

  abstract class Message(val msg: String)
  case class Public(override val msg: String) extends Message(msg)
  case class Private(override val msg: String) extends Message(msg)

  implicit def stringToMessageImplicit(s: String) = new Public(s)

  /**
   * Makes a new game object with the given player names
   */
  def apply(playerNames: => Seq[String]) = new Game(playerNames.map(Player(_)))

  /**
   * Returns the player object for the player who's turn it is. Does not change the game state
   */
  def currentPlayer = State[Game, Player] { g: Game => (g, g.players.head) }

  /**
   * Cycles the list of players so the player in front is last. Returns the new current player
   * also skips any eliminated players
   * also removed handmaid protection from the new front player
   */
  def endTurn = {
    def rotatePlayerOrder = State[Game, Player] { g: Game =>
      {
        //this would go into an infinate loop if all players are eliminated
        //that shouldn't ever happen, but just in case, crash instead of looping
        require(!g.players.forall(_.isEliminated))
        @annotation.tailrec
        def cyclePlayerList(l: Seq[Player]): Seq[Player] = {
          val l2 = l.tail ++ Seq(l.head)
          if (!l2.head.isEliminated) l2 else cyclePlayerList(l2)
        }
        val newPlayerList = cyclePlayerList(g.players)
        (g.copy(players = newPlayerList), newPlayerList.head)
      }
    }
    for {
      p <- rotatePlayerOrder
      p2 <- protectPlayer(p.name, false)
    } yield p2.get
  }

  /**
   * Puts the contents of the deck in random order, and returns the deck
   */
  def shuffle(implicit r: Random): State[Game, Seq[Card]] = State[Game, Seq[Card]] {
    g: Game =>
      {
        val newDeck = r.shuffle(g.deck)
        (g.copy(deck = newDeck), newDeck)
      }
  }

  /**
   * Starts a new match with the game's players. Throws away the old hand and discard states
   */
  def newMatch(firstPlayer: Option[String], r: Random) = State[Game, Unit] {
    g: Game =>
      {
        val freshPlayers = g.players.map(p => Player(name = p.name, score = p.score))
        val firstPlayerOrRandom = firstPlayer.getOrElse(r.shuffle(freshPlayers).head.name)
        val splitPlayers = freshPlayers.splitAt(freshPlayers.indexWhere(_.name == firstPlayerOrRandom))
        (Game(splitPlayers._2 ++ splitPlayers._1), {})
      }
  }

  /**
   * Removes the top card from the deck and returns it. The card will be put in the burn pile
   */
  def burnCard = State[Game, Card] {
    g: Game => (g.copy(deck = g.deck.tail, burnCard = Seq(g.deck.head)), g.deck.head)
  }

  /**
   * Removes the top card from the deck and returns it. The card will not be put in the discard pile
   */
  def popTopCardFromDeck = State[Game, Card] {
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
    popTopCardFromDeck.flatMap(addToVisibleDiscard).map(Seq(_))
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
    c <- popTopCardFromDeck
    player <- getPlayer(playerName)
    player2 <- updatePlayer(player.map(p => p.copy(hand = p.hand :+ c)))
  } yield player2

  /**
   * Remove the top card from the deck and add it to a player's hand. Then return that player object
   */
  def drawFromDeckOrBurnCard(playerName: String): State[Game, Option[Player]] = {
    def popDeckOrBurnPile = State[Game, Card] {
      (g: Game) => {
        println(g)
        if(g.deck.isEmpty) (g.copy(deck = g.burnCard.tail), g.burnCard.head) else (g.copy(deck = g.deck.tail), g.deck.head)
      }
    }
    for {
      c <- popDeckOrBurnPile
      player <- getPlayer(playerName)
      player2 <- updatePlayer(player.map(p => p.copy(hand = p.hand :+ c)))
    } yield player2
  }

  /**
   * replaces the player with the same name as the passed in player in the player list. Returns the new player
   * if None is passed in, jus return the same old game state and none
   */
  def updatePlayer(player: Option[Player]) = State[Game, Option[Player]] {
    g: Game =>
      player.map(
        p => (g.copy(players = g.players.updated(g.players.indexWhere(_.name == p.name), p)), Some(p))).getOrElse((g, None: Option[Player]))
  }

  /**
   * removes a card from the given players hand and puts it in the discard pile. Returns the discard pile
   * does not change the state and returns an empty list if an invalid player or card is selected
   */
  def playerDiscard(playerName: String, c: Card): State[Game, List[Card]] = {
    getPlayer(playerName).flatMap(_.flatMap({
      p =>
        if (p.hand.contains(c)) {
          Some(updatePlayer(Some(p.copy(hand = p.hand.diff(Seq(c))))).flatMap(_ => discard(c)))
        } else {
          None
        }
    }).getOrElse(State[Game, List[Card]](g => (g, Nil))))
  }

  /**
   * increments a players score, then returns that player. returns none and leaves the state the same if the player doesnt exist
   */
  def awardPoint(playerName: String): State[Game, Option[Player]] = {
    getPlayer(playerName).flatMap(player => {
      updatePlayer(player.map(p => p.copy(score = p.score + 1)))
    })
  }

  /**
   * Adds a card to the discard pile, then returns the discard pile
   */
  def discard(c: Card) = State[Game, List[Card]] {
    g: Game =>
      {
        val newDiscard = c :: g.discard
        (g.copy(discard = newDiscard), newDiscard)
      }
  }

  /**
   * returns the winning player, or None if there isn't one
   */
  def findWinner = State[Game, Option[Player]] {
    g: Game =>
      {
        val threshold = g.players.length match {
          case 2 => 7
          case 3 => 5
          case 4 => 4
          case _ => throw new AssertionError("Game can only have 2-4 players")
        }
        (g, g.players.find(_.score >= threshold))
      }
  }

  /**
   * begins a new match by restarting the deck, burning, dealing, and drawing a card for the first player
   */
  def startMatch(firstPlayer: Option[String] = None)(implicit r: Random) = {
    def burn3VisibleIfTwoPlayer = State[Game, Seq[Card]] {
      g: Game =>
        if (g.players.length == 2) {
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
      _ <- newMatch(firstPlayer, r)
      _ <- shuffle(r)
      _ <- burnCard
      _ <- burn3VisibleIfTwoPlayer
      _ <- dealFirstCards
      p <- currentPlayer
      _ <- drawCard(p.name)
    } yield ()
  }

  /**
   * sets the player's "protected" value to true or false. returns the new player, or None if player doesn't exist
   */
  def protectPlayer(playerName: String, isProtected: Boolean): State[Game, Option[Player]] = {
    for {
      p <- getPlayer(playerName)
      p2 <- updatePlayer(p.map(_.copy(isProtected = isProtected)))
    } yield p2
  }

  /**
   * sets the player's "eliminated" value to true or false. returns the new player, or None if player doesn't exist
   */
  def eliminatePlayer(playerName: String, isEliminated: Boolean): State[Game, Option[Player]] = {
    for {
      p <- getPlayer(playerName)
      p2 <- updatePlayer(p.map(_.copy(isEliminated = isEliminated)))
      //there shouldn't be a player without a name in the game, so this should do nothing if the card isn't there
      _ <- playerDiscard(p2.map(_.name).getOrElse(""), p2.flatMap(_.hand.headOption).getOrElse(Guard))
    } yield p2.map(_.copy(hand = Nil))
  }

  /**
   * Checks if the match had a winner, if it does, return that player. Otherwise return none
   */
  def findMatchWinner = State[Game, Option[Player]] {
    g: Game =>
      {
        val remainingPlayers = g.players.filter(!_.isEliminated)
        if (remainingPlayers.length == 1) {
          (g, Some(remainingPlayers.head))
        } else if (g.deck.isEmpty) {
          (g, Some(remainingPlayers.max))
        } else {
          (g, None)
        }
      }
  }

  /**
   * Checks if the match has a winner, If so, award them a point, return the player, and restart the match. Otherwise return none
   * Should be called at the end of each turn during game processing
   */
  def checkMatchOver(implicit r: Random): State[Game, Option[Player]] = {
    findMatchWinner.flatMap(
      _.map(p => awardPoint(p.name).
        flatMap(_ => startMatch(Some(p.name))(r)).
        map(_ => Some(p): Option[Player])).
        getOrElse(State.state(None)))
  }

  /**
   * Iterate through one turn of the game. This function will:
   * if the player name passed in matches the current player, discard that card from their hand (else don't change the state)
   * handle any effects from that discard
   * check to see if any player has won the match, if so iterate to a new match
   * check to see if any player has won the game
   * return a tuple with an optional winner of the match, an optional winner of the game, and a message for what happened that turn
   */
  def processTurn(playerName: String,
    discard: Card,
    targetName: Option[String] = None,
    guess: Option[Card] = None)
    (implicit r: Random): State[Game, Either[Message, Seq[Message]]] = {

    def maybeDiscard(b: Boolean, name: String, card: Card):State[Game, _] = if(b) playerDiscard(name, card) else State.state(None)
    def maybeEndTurn(b: Boolean):State[Game, _] = if(b) endTurn else State.state(None)
    def maybeDrawCard(b: Boolean, playerName: String):State[Game, _] = if(b) drawCard(playerName) else State.state(None)

    getPlayer(playerName).flatMap(playerOption =>
      playerOption.flatMap(p => {
        //this is kind of cheating, but we can tell if the player is the current player by seeing if they have 2 cards in their hand
        if (p.hand.length != 2) {
          Some(State.state(Left(new Private("It is not your turn")))): Option[State[Game, Either[Message, Seq[Message]]]]
        } else if(!p.hand.contains(discard)) {
          Some(State.state(Left(new Private("you to not have that card")))): Option[State[Game, Either[Message, Seq[Message]]]]
        } else {
          Some(for {
            actionResult <- discard.doAction(p, targetName, guess)
            _ <- maybeDiscard(actionResult.isRight, p.name, discard)
            _ <- maybeEndTurn(actionResult.isRight)
            nextPlayer <- currentPlayer
            matchWinner <- checkMatchOver(r)
            gameWinner <- findWinner
            _ <- maybeDrawCard(actionResult.isRight && matchWinner.isEmpty, nextPlayer.name)
          } yield (actionResult.right.map(m => {
            Seq[Option[Message]](Some(m),
              Some(s"It is ${nextPlayer.name}'s turn"),
              matchWinner.map(p => s"${p.name} has won the match!"),
              gameWinner.map(p => s"${p.name} has won the game!")
            ).flatten
          }))): Option[State[Game, Either[Message, Seq[Message]]]]
        }
      }).getOrElse(State.state(Left(new Private("Player not found or not in game")))))
  }

  /**
   * checks if everyone  other than the current player is protected or eliminated
   * this is important because cards like guard don't need a target if everyone is safe
   */
  def isEveryoneElseProtectedOrEliminated: State[Game, Boolean] = {
    currentPlayer.flatMap(p => State[Game, Boolean] {
      g: Game => (g, g.players.diff(Seq(p)).forall(pp => pp.isEliminated || pp.isProtected))
    })
  }
}
