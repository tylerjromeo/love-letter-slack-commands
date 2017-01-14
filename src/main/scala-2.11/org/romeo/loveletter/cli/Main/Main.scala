package org.romeo.loveletter.cli.Main

import java.lang.System

import org.romeo.loveletter.game.{Game, GameManager}
import org.romeo.loveletter.persistence.MemoryDataStore

import scala.io.StdIn
import scala.util.Random

/**
  * Created by tylerromeo on 1/13/17.
  */
object Main extends App {
  val gameId = "cli"
  implicit val random = new Random()
  val gameManager = new GameManager(new MemoryDataStore())
  var gameStarted: Boolean = false
  stdin foreach { command =>
    processCommand(command)
    if (gameStarted) {
      val gameInfo = gameManager.getGameInfo(gameId)
      val currentPlayer = gameManager.getCurrentPlayerName(gameId)
      val playersHand = gameManager.getHandInfo(gameId, currentPlayer)
      println(gameInfo)
      println(s"Your hand:\n$playersHand")
    }
  }

  def stdin: Stream[String] = StdIn.readLine("----------\nEnter Command:") match {
    case s if s == null => Stream.empty
    case s => s #:: stdin
  }

  def processCommand(s: String): Unit = {
    val split = s.split("""\s+""")
    split.toList match {
      case command :: cardName :: args if command == "play" || command == "discard" => {
        val target = args.headOption
        val guess = if (args.size >= 2) Some(args(1)) else None
        playCard(card = cardName,
          target = target,
          guess = guess)
      }
      case command :: args if command == "start" => startGame(players = args)
      case command :: _ if command == "quit" => sys.exit(0)
      case command :: _ if command == "help" => printHelp()
      case _ => printHelp()
    }
  }

  def printHelp(): Unit = {
    val help =
      """
        |Rules are online at: http://www.alderac.com/tempest/files/2012/09/Love_Letter_Rules_Final.pdf
        |`help` to get this help
        |`start [player names]` to start a new game
        |`quit` to end the game
        |`play [card name] [?target] [?guess]` to play a card. (play can be omitted)
        |`discard` is equivalent to `play`
        |Source for bot at: https://github.com/tylerjromeo/love-letter-slack-commands
      """.stripMargin
    println(help)
  }

  def startGame(players: List[String]): Unit = {
    gameManager.startGame(gameId, players) match {
      case Left(m) => println(s"Could not start game: $m")
      case Right(game) => {
        gameStarted = true
        println(s"Game started")
      }
    }
  }

  def playCard(card: String, target: Option[String], guess: Option[String]): Unit = {
    gameManager.takeTurn(gameId, gameManager.getCurrentPlayerName(gameId), card, target, guess) match {
      case Left(m) => println(s"Error: ${m.msg}")
      case Right(m) => println(m.map(_.msg).mkString("\n"))
    }
  }

}
