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
      case s => println(s)
    }
  }

}
