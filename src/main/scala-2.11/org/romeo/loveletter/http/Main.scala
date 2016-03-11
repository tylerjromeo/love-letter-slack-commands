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
import akka.util.Timeout
import akka.pattern.ask
import akka.io.IO
import spray.http.{MediaTypes, StatusCodes}
import spray.json._
import spray.routing.SimpleRoutingApp
import scala.concurrent.duration._
import scala.concurrent.Future
import scala.util.{Properties, Success, Failure, Try}
import org.romeo.loveletter.persistence.MemoryDataStore
import spray.can.Http
import spray.http._
import spray.client.pipelining._
import spray.httpx.SprayJsonSupport
import spray.json.RootJsonFormat
import org.romeo.loveletter.game._
import org.romeo.loveletter.game.Game._


case class SlackResponse(privateMessage: Boolean, text: String)

case class MultiSlackResponse(pubMessages: SlackResponse, privMessages: SlackResponse, isGameOver: Boolean = false) {
  def isPrivateOnly: Boolean = this.pubMessages.text.isEmpty
  def isPublicOnly: Boolean = this.privMessages.text.isEmpty
}

object MultiSlackResponse {
  def fromSlackResponses(responses: Seq[SlackResponse]): MultiSlackResponse = {
    def isGameOver(sr: SlackResponse) = sr.text.endsWith(" has won the game!") //TODO: refactor the engine to do this in a less ugly way
    responses.foldLeft(MultiSlackResponse(SlackResponse(false, ""), SlackResponse(true, ""), false))((msr, sr) => {
      if(sr.privateMessage) {
        MultiSlackResponse(msr.pubMessages, SlackResponse(true, s"${msr.privMessages.text}\n${sr.text}"), msr.isGameOver || isGameOver(sr))
      } else {
        MultiSlackResponse(SlackResponse(false, s"${msr.pubMessages.text}\n${sr.text}"), msr.privMessages, msr.isGameOver || isGameOver(sr))
      }
    })
  }
}

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
  import scala.concurrent.ExecutionContext.Implicits.global
  implicit def messageToSlackResponseImplicit(m: Message) = {
    if(m.isInstanceOf[Private]) SlackResponse(true, m.msg) else SlackResponse(false, m.msg)
  }
  implicit def SlackResponseToMultiSlackResponse(sr: SlackResponse) = {
    if(sr.privateMessage) {
      MultiSlackResponse(SlackResponse(false, ""), sr, false)
    } else {
      MultiSlackResponse(sr, SlackResponse(true, ""), false)
    }
  }
  implicit val system = ActorSystem("my-system")
  implicit val timeout: Timeout = Timeout(15.seconds)

  val gameManager = new GameManager(new MemoryDataStore[Game]())(new Random())

  val teamToken = "K6kOMrLxkZfHoZvKIbE2Guzm"

  val helpText = """
                  |Rules are online at: http://www.alderac.com/tempest/files/2012/09/Love_Letter_Rules_Final.pdf
                  |`/love help` to get this help
                  |`/love start [player names]` to start a new game
                  |`/love quit` to end the game in the channel
                  |`/love hand` to see your hand
                  |`/love status` to see all available information on the board
                  |`/love play [card name] [?target] [?guess]` to play a card. (play can be omitted)
                  |`/love discard` is equivalent to `/love play`
                  |Source for bot at: https://github.com/tylerjromeo/love-letter-slack-commands
                """.stripMargin

  //store response urls per player-channel, so we can tell them their hand at the beginning of each turn
  val responseUrlMap = scala.collection.mutable.Map[String, String]()

  def sendToSlackUrl(responseUrl: String, message: SlackResponse): Unit = {
    val pipeline: HttpRequest => Future[HttpResponse] = sendReceive
    pipeline(Post(Uri(responseUrl), message))
  }

  def runCommand(text: String, channelName: String, userName: String, responseUrl: String): MultiSlackResponse = {
    def playCard(channelName: String, cardName: String, target: Option[String], guess: Option[String]): MultiSlackResponse = {
      val msr = gameManager.takeTurn(channelName, userName, cardName, target, guess) match {
        case Left(message) => SlackResponseToMultiSlackResponse(message)
        case Right(messages) => {
          val nextPlayer = gameManager.getCurrentPlayerName(channelName)
          responseUrlMap.get(channelName + nextPlayer).foreach(url => {
            sendToSlackUrl(url, SlackResponse(true, gameManager.getHandInfo(channelName, nextPlayer)))
          })
          MultiSlackResponse.fromSlackResponses(messages.map(messageToSlackResponseImplicit(_)))
        }
      }
      if(msr.isGameOver) gameManager.abortGame(channelName)
      msr
    }
    val params = text.split("\\s+")
    params(0) match {
      case "start" if (params.length >= 3 && params.length <= 5)=> gameManager.startGame(channelName, params.tail) match {
        case Left(message) => SlackResponse(true, message)
        case Right(game) => {
          val player1 = Game.currentPlayer.eval(game).name
          SlackResponse(false, s"Game started!\nIt is $player1's turn")
        }
      }
      case "quit" => {
        SlackResponse(false, gameManager.abortGame(channelName).merge)
      }
      case "status" => SlackResponse(false, gameManager.getGameInfo(channelName))
      case "hand" => SlackResponse(true, gameManager.getHandInfo(channelName, userName))
      case "play"|"discard" if params.length >= 2 => {
        val cardName = params(1)
        val target = if(params.isDefinedAt(2)) Some(params(2)) else None
        val guess = if(params.isDefinedAt(3)) Some(params(3)) else None
        playCard(channelName, cardName, target, guess)
      }
      case x if(Deck.isCardName(x)) => {
        val cardName = params(0)
        val target = if(params.isDefinedAt(1)) Some(params(1)) else None
        val guess = if(params.isDefinedAt(2)) Some(params(2)) else None
        playCard(channelName, cardName, target, guess)
      }
      case _ => SlackResponse(true, helpText)
    }
  }

  startServer(interface = "0.0.0.0", port = Properties.envOrElse("PORT", "8080").toInt) {
    pathSingleSlash {
      post {
        formFields('token, 'team_id, 'team_domain, 'channel_id, 'channel_name, 'user_id, 'user_name, 'command, 'text, 'response_url) {
          (token, teamId, teamDomain, channelId, channelName, userId, userName, command, text, responseUrl) =>
            validate(token == teamToken, "Request token does not match team") {
            respondWithMediaType(MediaTypes.`application/json`) {
              complete {
                responseUrlMap.put(channelName + userName, responseUrl)
                val responses = runCommand(text, channelName, userName, responseUrl)
                //if the responses are only of one type, respond with it.
                //Otherwise respond with the public message and send the private over the url
                if(responses.isPrivateOnly) {
                  responses.privMessages
                } else if(responses.isPublicOnly) {
                  responses.pubMessages
                } else {
                  sendToSlackUrl(responseUrl, responses.privMessages)
                  responses.pubMessages
                }
              }
            }
          }
        }
      }
    }
  }
}
