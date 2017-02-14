package org.romeo.loveletter.http

/**
  * Created by tylerromeo on 2/13/17.
  */
object Properties {

  def port: Int = scala.util.Properties.envOrElse("PORT", "8080").toInt
  def commandToken: Option[String] = scala.util.Properties.envOrNone("SLACK_COMMAND_TOKEN")

}
