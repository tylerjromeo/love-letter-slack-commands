package org.romeo.loveletter.http

import scala.util.Random
import scalaz.State

import org.romeo.loveletter.game._
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

  def abortGame(gameId: String): Unit = {
    datastore.remove(gameId)
  }

  def takeTurn(gameId: String, player: String, cardName: String, targetPlayer: Option[String], cardGuess: Option[String]): Either[String, String] = {
    Deck.getCardByName(cardName).map(card =>
      datastore.get(gameId).map(game => {
        Game.processTurn(player, card, targetPlayer, cardGuess.flatMap(Deck.getCardByName(_))).eval(game)
      }).getOrElse((None, None, Left("Game not found")))
    ).map(result => {
      val matchWinnerMessage = result._1.map(w => s"\n$w has won the match!").getOrElse("")
      val gameWinnerMessage = result._2.map(w => s"\n$w has won the game!").getOrElse("")
      result._3.right.map(_ + matchWinnerMessage + gameWinnerMessage)
    }).getOrElse(Left(s"$cardName doesn't exist in the game"))
  }

  def getGameInfo(gameId: String): String = {
    datastore.get(gameId).map(game => {
      val (currentPlayer, allPlayers, topDiscard, cardsLeft) = (for {
        currentPlayer <- Game.currentPlayer
        allPlayers <- State[Game, Seq[Player]](g => (g, g.players))
        topDiscard <- State[Game, Option[Card]](g => (g, g.discard.headOption))
        cardsLeft <- State[Game, Int](g => (g, g.deck.length))
      } yield (currentPlayer.name, allPlayers, topDiscard.map(_.name), cardsLeft)).eval(game)
      Seq(
        s"It is ${currentPlayer}'s turn",
        s"There are $cardsLeft cards in the deck",
        topDiscard.map(c => s"the discard pile has a $c on top").getOrElse("The discard pile is empty"),
        allPlayers.map(p => s"${p.name} has ${p.score} points").mkString("\n")
      ).mkString("\n")
    }).getOrElse(s"No game with id $gameId exists")
  }
}
