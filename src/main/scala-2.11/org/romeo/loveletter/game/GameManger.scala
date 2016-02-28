package org.romeo.loveletter.game

import scala.util.Random
import scalaz.State

import org.romeo.loveletter.game.Game._
import org.romeo.loveletter.persistence.Datastore

class GameManager(val datastore: Datastore[Game])(implicit val r: Random) {

  def startGame(gameId: String, players:Seq[String]): Either[String, Game] = {
    if(datastore.exists(gameId)) {
      Left(s"Game with id ${gameId} already exists!")
    } else {
      val game = Game.startMatch(None).exec(Game(players))
      datastore.put(gameId, game)
      Right(game)
    }
  }

  def abortGame(gameId: String): Either[String, String] = {
    if(datastore.exists(gameId)) {
      datastore.remove(gameId)
      Right("Game ended!")
    } else {
      Left("Game not found")
    }
  }

  def takeTurn(gameId: String, player: String, cardName: String, targetPlayer: Option[String], cardGuess: Option[String]): Either[Message, Seq[Message]] = {
    Deck.getCardByName(cardName).map(card =>
      datastore.get(gameId).map(game => {
        val (newGame, result) = Game.processTurn(player, card, targetPlayer, cardGuess.flatMap(Deck.getCardByName(_))).apply(game)
        datastore.put(gameId, newGame)
        result
      }).getOrElse(Left(new Private("Game not found")))
    ).getOrElse(Left(new Private(s"$cardName doesn't exist in the game")))
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
        s"It is ${currentPlayer}'s turn",
        s"There are $cardsLeft cards in the deck",
        if(!visibleDiscard.isEmpty) s"${visibleDiscard.map(_.name).mkString(", ")} are visibly discarded" else "There are no visible burn cards",
        topDiscard.map(c => s"the discard pile has a $c on top").getOrElse("The discard pile is empty"),
        allPlayers.map(p => s"${p.name} has ${p.score} points").mkString("\n")
      ).mkString("\n")
    }).getOrElse(s"No game with id $gameId exists")
  }

  def getHandInfo(gameId: String, playerName: String): String = {
    datastore.get(gameId).flatMap(game => {
      val player = Game.getPlayer(playerName).eval(game)
      player.map(_.hand.mkString("\n"))
    }).getOrElse(s"No game with id $gameId exists")
  }
}
