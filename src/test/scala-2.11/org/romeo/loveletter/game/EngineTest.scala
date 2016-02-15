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
    Game.currentPlayer.eval(game).name should be (players.head)
  }

  "A game's first player" should "change when a turn is ended" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    var game = Game(players);
    Game.endTurn.exec(game).players.head.name should be (players(1))
  }

  "A game" should "be back to its initial state after a full round of turns" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    var game = Game(players);
    def FullRoundCycle() = for {
      p1 <- Game.currentPlayer
      p2 <- Game.endTurn
      p3 <- Game.endTurn
      p4 <- Game.endTurn
      _ <- Game.endTurn
    } yield Seq(p1, p2, p3, p4)
    //after 4 turns change the state should be back to where it started
    val (newGame, newPlayers) = FullRoundCycle()(game)
    newGame should be (game)
    newPlayers.map(_.name) should be (players)
  }
}
