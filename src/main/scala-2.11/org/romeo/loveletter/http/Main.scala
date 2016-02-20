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
        JsObject(Map("response_type" -> JsString("in_channel"), "text" -> JsString(r.text)))
      } else {
        JsObject(Map("text" -> JsString(r.text)))
      }
    }

    def read(value: JsValue) = {
      value.asJsObject.getFields("response_type", "text") match {
        case Seq(JsString(response_type), JsString(text)) => new SlackResponse(true, text)
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

  startServer(interface = "localhost", port = Properties.envOrElse("PORT", "8080").toInt) {
    pathSingleSlash {
      post {
        formFields('token, 'team_id, 'team_domain, 'channel_id, 'channel_name, 'user_id, 'user_name, 'command, 'text, 'response_url) {
          (token, teamId, teamDomain, channelId, channelName, userId, userName, command, test, responseUrl) =>
            validate(token == teamToken, "Request token does not match team") {
            respondWithMediaType(MediaTypes.`application/json`) {
              complete(SlackResponse(false, "test hello")) //TODO: connect routes to game
            }
          }            
        }
      }
    }
  }
}
