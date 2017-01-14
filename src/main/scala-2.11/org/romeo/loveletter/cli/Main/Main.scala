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
    s match {
      case s if s == "exit" => sys.exit(0)
      case "help" => printHelp()
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
        | prefix any command with a player name in parens to act as that player. If this is omitted the player whose turn it is will act
        |Source for bot at: https://github.com/tylerjromeo/love-letter-slack-commands
      """.stripMargin
    println(help)
  }

}
