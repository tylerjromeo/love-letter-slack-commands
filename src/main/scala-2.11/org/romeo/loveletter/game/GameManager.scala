package org.romeo.loveletter.game

import scala.util.Random
import scalaz.State

import org.romeo.loveletter.game.Game._
import org.romeo.loveletter.persistence.Datastore

class GameManager(val datastore: Datastore[Game], rand: Random) {

  implicit val r: Randomizer = Randomizer(
    shuffleDeck = (s: Seq[Card]) => rand.shuffle(s),
    choosePlayer = (s: Seq[Player]) => rand.shuffle(s).head
  )

  def startGame(gameId: String, players: Seq[String]): Either[String, Game] = {
    if (datastore.exists(gameId)) {
      Left(s"Game with id $gameId already exists!")
    } else {
      val game = Game.startMatch(None).exec(Game(players))
      datastore.put(gameId, game)
      Right(game)
    }
  }

  def abortGame(gameId: String): Either[String, String] = {
    if (datastore.exists(gameId)) {
      datastore.remove(gameId)
      Right("Game ended!")
    } else {
      Left("Game not found")
    }
  }

  def takeTurn(gameId: String, player: String, cardName: String, targetPlayer: Option[String], cardGuess: Option[String]): Either[Message, Seq[Message]] = {
    Deck.getCardByName(cardName).map(card =>
      datastore.get(gameId).map(game => {
        val (newGame, result) = Game.processTurn(player, card, targetPlayer, cardGuess.flatMap(Deck.getCardByName)).apply(game)
        datastore.put(gameId, newGame)
        result match {
          case PlayError(message) => {
            Left(Private(message))
          }
          case GameOver(lastTurnResult, matchWinner, gameWinner) => {
            Right(Seq[Message](
              lastTurnResult,
              Public(s"!!! $matchWinner has won the match and gets a point !!!"),
              Public(s"!!!!!! $gameWinner has won the game !!!!!!")
            ))
          }
          case MatchOver(lastTurnResult, matchWinner, nextPlayer) => {
            Right(Seq[Message](
              lastTurnResult,
              Public(s"!!! $matchWinner has won the match and gets a point !!!"),
              Public(s"!!!!!! A new game has started, it is $nextPlayer's turn !!!!!!")
            ))
          }
          case NextTurn(lastTurnResult, nextPlayer) => {
            Right(Seq[Message](
              lastTurnResult,
              Public(s"It is $nextPlayer's turn")
            ))
          }
        }
      }).getOrElse(Left(Private("Game not found")))
    ).getOrElse(Left(Private(s"$cardName doesn't exist in the game")))
  }

  def getGameInfo(gameId: String): String = {
    datastore.get(gameId).map(game => {
      val (currentPlayer, allPlayers, topDiscard, visibleDiscard, cardsLeft) = (for {
        currentPlayer <- Game.currentPlayer
        allPlayers <- State[Game, Seq[Player]](g => (g, g.players))
        topDiscard <- State[Game, Option[Card]](g => (g, g.discard.headOption))
        visibleDiscard <- State[Game, Seq[Card]](g => (g, g.visibleDiscard))
        cardsLeft <- State[Game, Int](g => (g, g.deck.length))
      } yield (currentPlayer.name, allPlayers, topDiscard.map(_.name), visibleDiscard, cardsLeft)).eval(game)
      Seq(
        s"It is $currentPlayer's turn",
        s"There are $cardsLeft cards in the deck",
        if (visibleDiscard.nonEmpty) s"${visibleDiscard.map(_.name).mkString(", ")} are visibly discarded" else "There are no visible burn cards",
        topDiscard.map(c => s"the discard pile has a $c on top").getOrElse("The discard pile is empty"),
        allPlayers.map(p => s"${p.name} has ${p.score} points").mkString("\n")
      ).mkString("\n")
    }).getOrElse(s"No game with id $gameId exists")
  }

  def getCurrentPlayerName(gameId: String): String = {
    datastore.get(gameId).map(Game.currentPlayer.eval(_).name).getOrElse(s"No game with id $gameId exists")
  }

  def getHandInfo(gameId: String, playerName: String): String = {
    datastore.get(gameId).flatMap(game => {
      val player = Game.getPlayer(playerName).eval(game)
      player.map(_.hand.mkString("\n"))
    }).getOrElse(s"No game with id $gameId exists")
  }
}
