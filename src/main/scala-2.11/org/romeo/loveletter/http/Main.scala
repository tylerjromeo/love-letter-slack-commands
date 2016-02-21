package org.romeo.loveletter.http
/**
 * User: tylerromeo
 * Date: 2/8/16
 * Time: 2:20 PM
 *
 */

import akka.actor.ActorSystem
import spray.http.{MediaTypes, StatusCodes}
import spray.json._
import spray.routing.SimpleRoutingApp
import scala.util.{Properties, Success, Failure, Try}
import org.romeo.loveletter.persistence.MemoryDataStore
import spray.httpx.SprayJsonSupport
import spray.json.RootJsonFormat

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
  implicit val system = ActorSystem("my-system")
  import SlackResponseJsonSupport._

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

  def runCommand(text: String): SlackResponse = {
    val params = text.split("\\s+")
    params(0) match {
      case "start" => SlackResponse(false, "start NOT YET IMPLEMENTED")
      case "quit" => SlackResponse(false, "quit NOT YET IMPLEMENTED")
      case "status" => SlackResponse(false, "status NOT YET IMPLEMENTED")
      case "hand" => SlackResponse(false, "hand NOT YET IMPLEMENTED")
      case "play" => SlackResponse(false, "play NOT YET IMPLEMENTED")
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
              complete(runCommand(text)) //TODO: connect routes to game
            }
          }
        }
      }
    }
  }
}
