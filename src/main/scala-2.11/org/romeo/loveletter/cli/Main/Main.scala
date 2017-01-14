package org.romeo.loveletter.cli.Main

import java.lang.System

import scala.io.StdIn

/**
  * Created by tylerromeo on 1/13/17.
  */
object Main extends App {
  stdin foreach processCommand

  def stdin: Stream[String] = StdIn.readLine("Enter Command:") match {
    case s if s == null => Stream.empty
    case s => s #:: stdin
  }

  def processCommand(s: String): Unit = {
    val split = s.split("""\s+""")
    split.toList match {
      case command :: _ if command == "help" => printHelp()
      case command :: args if command == "start" => startGame(players = args)
      case command :: _ if command == "quit" => sys.exit(0)
      case command :: _ if command == "hand" => showHand()
      case command :: _ if command == "status" => showStatus()
      case command :: cardName :: args if command == "play" || command == "discard" => {
        val target = args.headOption
        val guess = if(args.size >= 2) Some(args(1)) else None
        playCard(card = cardName,
          target = target,
          guess = guess)
      }
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
        |`hand` to see your hand
        |`status` to see all available information on the board
        |`play [card name] [?target] [?guess]` to play a card. (play can be omitted)
        |`discard` is equivalent to `play`
        |Source for bot at: https://github.com/tylerjromeo/love-letter-slack-commands
      """.stripMargin
    println(help)
  }

  def startGame(players: List[String]): Unit = ???

  def showHand(): Unit = ???

  def showStatus(): Unit = ???

  def playCard(card: String, target: Option[String], guess: Option[String]):Unit = ???

}
