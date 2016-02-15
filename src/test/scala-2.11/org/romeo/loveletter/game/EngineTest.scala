import scala.language.postfixOps
import scala.util.Random
import org.scalatest._
import org.romeo.loveletter.game._

class EngineSpec extends FlatSpec with Matchers {

  behavior of "A game"

  it should "fail to start with too few players" in {
    val tooFewPlayers = Seq("Tyler")
    an [IllegalArgumentException] should be thrownBy Game(tooFewPlayers)
  }
  it should "fail to start with too many players" in {
    val tooManyPlayers = Seq("Tyler", "Kevin", "Morgan", "Trevor", "Jeff")
    an [IllegalArgumentException] should be thrownBy Game(tooManyPlayers)
  }

  behavior of "The first player"

  it should "be the current player" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game(players);
    Game.currentPlayer.eval(game).name should be (players.head)
  }

  it should "change when a turn is ended" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game(players);
    Game.endTurn.exec(game).players.head.name should be (players(1))
  }

  it should "be back to its initial state after a full round of turns" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game(players);
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

  behavior of "The game deck"

  it should "be put in random order when shuffled" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game(players)
    val r1 = new Random(1337)
    val r2 = new Random(1337)

    val (newGame, shuffledDeck) = Game.shuffle(r1)(game)
    newGame.deck should be (shuffledDeck)
    shuffledDeck should not be (Deck.cards)
    shuffledDeck should contain theSameElementsAs Deck.cards
    shuffledDeck should be (r2.shuffle(Deck.cards))
  }

  it should "lose its top card when burnCard is called" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game(players)

    def shuffleAndBurn(r: Random) = for {
      deck <- Game.shuffle(r)
      burntCard <- Game.burnCard
    } yield (deck, burntCard)

    val (newGame, (newDeck, burntCard)) = shuffleAndBurn(new Random(1338))(game)
    newGame.deck should be (newDeck.tail)
    newGame.discard should be (game.discard)
    newGame.visibleDiscard should be (game.visibleDiscard)
  }

  it should "return the burned card when burncard is called" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game(players)

    def shuffleAndBurn(r: Random) = for {
      deck <- Game.shuffle(r)
      burntCard <- Game.burnCard
    } yield (deck, burntCard)

    val (newGame, (newDeck, burntCard)) = shuffleAndBurn(new Random(1339))(game)
    Seq(burntCard) ++ newGame.deck should be (newDeck)
  }

  it should "not have any change in the discard piles after burning" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game(players)

    def shuffleAndBurn(r: Random) = for {
      deck <- Game.shuffle(r)
      burntCard <- Game.burnCard
    } yield (deck, burntCard)

    val newGame = shuffleAndBurn(new Random(1340)).exec(game)
    newGame.discard should be (game.discard)
    newGame.visibleDiscard should be (game.visibleDiscard)
  }

  it should "show the burned card if burnCardVisible is called" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game(players)

    def shuffleAndBurn(r: Random) = for {
      _ <- Game.shuffle(r)
      burntCard <- Game.burnCardVisible
    } yield burntCard

    val (newGame, burntCard) = shuffleAndBurn(new Random(1341))(game)
    newGame.discard should be (game.discard)
    newGame.visibleDiscard should contain only (burntCard)
  }

  it should "no longer have its top card when a player draws" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game(players)

    def shuffleAndDraw(r: Random) = for {
      startingDeck <- Game.shuffle(r)
      currentPlayer <- Game.currentPlayer
      newPlayer <- Game.drawCard(currentPlayer)
    } yield startingDeck

    val (newGame, startingDeck) = shuffleAndDraw(new Random(1342))(game)
    newGame.deck should be (startingDeck.tail)
  }

  behavior of "A player"

  it should "gain the top card of the deck when they draw" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game(players)

    def shuffleAndDraw(r: Random) = for {
      startingDeck <- Game.shuffle(r)
      currentPlayer <- Game.currentPlayer
      newPlayer <- Game.drawCard(currentPlayer)
    } yield (startingDeck, newPlayer)

    val (newGame, (startingDeck, newPlayer)) = shuffleAndDraw(new Random(1343))(game)
    newGame.players(0) should be (newPlayer)
    newPlayer.hand should contain only (startingDeck.head)
  }

  it should "not gain cards when another player draws" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game(players)

    def shuffleAndDraw(r: Random) = for {
      startingDeck <- Game.shuffle(r)
      currentPlayer <- Game.currentPlayer
      newPlayer <- Game.drawCard(currentPlayer)
    } yield ()

    val newGame = shuffleAndDraw(new Random(1344)).exec(game)
    newGame.players.tail.foreach(p => {
      p.hand shouldBe empty
    })
  }
}
