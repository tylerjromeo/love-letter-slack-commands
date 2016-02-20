package org.romeo.loveletter.http

import scala.util.Random
import scalaz.State

import org.romeo.loveletter.game.Game
import org.romeo.loveletter.game.Deck
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
}
