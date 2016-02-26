package org.romeo.loveletter.http
/**
 * User: tylerromeo
 * Date: 2/8/16
 * Time: 2:20 PM
 *
 */

import scala.language.implicitConversions
import scala.util.Random
import akka.actor.ActorSystem
import spray.http.{MediaTypes, StatusCodes}
import spray.json._
import spray.routing.SimpleRoutingApp
import scala.util.{Properties, Success, Failure, Try}
import org.romeo.loveletter.persistence.MemoryDataStore
import spray.httpx.SprayJsonSupport
import spray.json.RootJsonFormat

import org.romeo.loveletter.game._
import org.romeo.loveletter.game.Game._

case class SlackResponse(privateMessage: Boolean, text: String)

object SlackResponseJsonSupport extends DefaultJsonProtocol with SprayJsonSupport {
  implicit object SlackResponseJsonFormats extends RootJsonFormat[SlackResponse] {
    def write(r: SlackResponse) = {
      if(r.privateMessage) {
        JsObject(Map("text" -> JsString(r.text)))
      } else {
        JsObject(Map("response_type" -> JsString("in_channel"), "text" -> JsString(r.text)))
      }
    }

    def read(value: JsValue) = {
      value.asJsObject.getFields("response_type", "text") match {
        case Seq(JsString(response_type), JsString(text)) => new SlackResponse(false, text)
        case Seq(JsString(text)) => new SlackResponse(false, text)
        case _ => throw new DeserializationException("SlackResponse expected")
      }
    }
  }
}

object Main extends App with SimpleRoutingApp {
  import SlackResponseJsonSupport._
  implicit def messageToSlackResponseImplicit(m: Message) = {
    if(m.isInstanceOf[Private]) SlackResponse(true, m.msg) else SlackResponse(false, m.msg)
  }
  implicit val system = ActorSystem("my-system")

  val gameManager = new GameManager(new MemoryDataStore[Game]())(new Random())

  val teamToken = "K6kOMrLxkZfHoZvKIbE2Guzm"

  val helpText = """
    Rules are online at: http://www.alderac.com/tempest/files/2012/09/Love_Letter_Rules_Final.pdf
    `/love help` to get this help
    `/love start [player names]` to start a new game
    `/love quit` to end the game in the channel
    `/love hand` to see your hand
    `/love status` to see all available information on the board
    `/love play [card name] [?target] [?guess]` to play a card
    Source for bot at: https://github.com/tylerjromeo/love-letter-slack-commands
  """

  def runCommand(text: String, channelName: String, userName: String): SlackResponse = {
    val params = text.split("\\s+")
    params(0) match {
      case "start" if (params.length >= 3 && params.length <= 5)=> gameManager.startGame(channelName, params.tail) match {
        case Left(message) => SlackResponse(true, message)
        case Right(_) => SlackResponse(false, "Game started!")
      }
      case "quit" => {
        gameManager.abortGame(channelName)
        SlackResponse(false, "Game ended!")
      }
      case "status" => SlackResponse(false, gameManager.getGameInfo(channelName))
      case "hand" => SlackResponse(true, gameManager.getHandInfo(channelName, userName))
      case "play" if params.length >= 2 => {
        val cardName = params(1)
        val target = if(params.isDefinedAt(2)) Some(params(2)) else None
        val guess = if(params.isDefinedAt(3)) Some(params(3)) else None
        gameManager.takeTurn(channelName, userName, cardName, target, guess) match {
          case Left(message) => message
          case Right(messages) => messages.foldLeft(SlackResponse(false, ""))((sm, m) => SlackResponse(false, sm.text + "\n" + m.msg))
        }
      }
      case _ => SlackResponse(true, helpText)
    }
  }

  startServer(interface = "localhost", port = Properties.envOrElse("PORT", "8080").toInt) {
    pathSingleSlash {
      post {
        formFields('token, 'team_id, 'team_domain, 'channel_id, 'channel_name, 'user_id, 'user_name, 'command, 'text, 'response_url) {
          (token, teamId, teamDomain, channelId, channelName, userId, userName, command, text, responseUrl) =>
            validate(token == teamToken, "Request token does not match team") {
            respondWithMediaType(MediaTypes.`application/json`) {
              complete(runCommand(text, channelName, userName))
            }
          }
        }
      }
    }
  }
}
