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

object Main extends App with SimpleRoutingApp {
  implicit val system = ActorSystem("my-system")

  startServer(interface = "localhost", port = Properties.envOrElse("PORT", "8080").toInt) {
    pathSingleSlash {
      post {
        formFields('token, 'team_id, 'team_domain, 'channel_id, 'channel_name, 'user_id, 'user_name, 'command, 'text, 'response_url) {
          (token, teamId, teamDomain, channelId, channelName, userId, userName, command, test, responseUrl) =>
            respondWithMediaType(MediaTypes.`application/json`) {
              complete("{\"item\": \"test\"}") //TODO: connect routes to game
            }
        }
      }
    }
  }
}
