import scala.language.postfixOps
import org.scalatest._
import org.romeo.loveletter.game.Game

class EngineSpec extends FlatSpec with Matchers {

  "A game" should "fail to start with the wrong number of players" in {
    val tooFewPlayers = Seq("Tyler")
    val tooManyPlayers = Seq("Tyler", "Kevin", "Morgan", "Trevor", "Jeff")
    an [IllegalArgumentException] should be thrownBy Game(tooFewPlayers)
    an [IllegalArgumentException] should be thrownBy Game(tooManyPlayers)
  }

  "A game" should "Have the first player as the current player" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game(players);
    Game.currentPlayer(game).name should be (players.head)
  }

  "A game's first player" should "change when a turn is ended" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    var game = Game(players);
    Game.currentPlayer(game).name should be (players.head)
    game = Game.endTurn(game)
    Game.currentPlayer(game).name should be (players(1))
    game = Game.endTurn(game)
    Game.currentPlayer(game).name should be (players(2))
    game = Game.endTurn(game)
    Game.currentPlayer(game).name should be (players(3))
    game = Game.endTurn(game)
    Game.currentPlayer(game).name should be (players.head)
  }
}
