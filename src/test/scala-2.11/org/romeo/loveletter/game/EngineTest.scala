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

  behavior of "A match"

  it should "have 2 cards for the current player" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game.startMatch(new Random(9191)).exec(Game(players));

    val currentPlayer = Game.currentPlayer.eval(game);
    currentPlayer.hand should have length 2
  }

  it should "have 1 for player whose turn it isn't when starting a game" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game.startMatch(new Random(9192)).exec(Game(players));

    val currentPlayer = Game.currentPlayer.eval(game);
    (game.players.diff(Seq(currentPlayer))).foreach(_.hand should have length 1)
  }

  it should "have the same number of cards in the game as were in the deck (minus burn card)" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game.startMatch(new Random(9193)).exec(Game(players));

    val cardsInPlay = game.players.foldLeft(0)((acc, p) => acc + p.hand.length) + game.deck.length + game.visibleDiscard.length + game.discard.length
    cardsInPlay + 1 should be (org.romeo.loveletter.game.Deck.cards.length)
  }

  it should "have the same number of cards in the game as were in the deck (minus burn card) for a 2 player game" in {
    val players = Seq("Tyler", "Kevin")
    val game = Game.startMatch(new Random(9194)).exec(Game(players));

    val cardsInPlay = game.players.foldLeft(0)((acc, p) => acc + p.hand.length) + game.deck.length + game.visibleDiscard.length + game.discard.length
    cardsInPlay + 1 should be (org.romeo.loveletter.game.Deck.cards.length)
  }

  it should "have 3 visible discards in a 2 player game" in {
    val players = Seq("Tyler", "Kevin")
    val game = Game.startMatch(new Random(9195)).exec(Game(players));

    game.visibleDiscard should have length (3)
  }

  it should "have no visible discards in a non 2 player game" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game.startMatch(new Random(9196)).exec(Game(players));

    game.visibleDiscard should have length (0)
  }

//TODO
  it should "have all players not eliminated and not protected, even if they were in the previous match"

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
    newGame.visibleDiscard should contain theSameElementsAs (burntCard)
  }

  it should "no longer have its top card when a player draws" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game(players)

    def shuffleAndDraw(r: Random) = for {
      startingDeck <- Game.shuffle(r)
      _ <- Game.drawCard(players.head)
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
      newPlayer <- Game.drawCard(players.head)
    } yield (startingDeck, newPlayer)

    val (newGame, (startingDeck, newPlayer)) = shuffleAndDraw(new Random(1343))(game)
    newGame.players(0) should be (newPlayer.get)
    newPlayer.get.hand should contain only (startingDeck.head)
  }

  it should "not gain cards when another player draws" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game(players)

    def shuffleAndDraw(r: Random) = for {
      _ <- Game.shuffle(r)
      _ <- Game.drawCard(players.head)
    } yield ()

    val newGame = shuffleAndDraw(new Random(1344)).exec(game)
    newGame.players.tail.foreach(p => {
      p.hand shouldBe empty
    })
  }

  it should "no longer have a card in their hand after discarding" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game(players)

    def shuffleDrawAndDiscard(r: Random) = for {
      _ <- Game.shuffle(r)
      player <- Game.drawCard(players.head)
      _ <- Game.playerDiscard(players.head, player.get.hand.head)
    } yield player

    val (newGame, player) = shuffleDrawAndDiscard(new Random(1345))(game)
    newGame.players.head.hand shouldBe empty
    player.get.hand should have size 1
  }

  it should "start with 0 points" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game(players)

    game.players.foreach(_.score should be (0))
  }

  it should "have 1 more point when a point is awarded" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game(players)

    def giveSomePoints = for {
      _ <- Game.awardPoint(players(0))
      _ <- Game.awardPoint(players(1))
      _ <- Game.awardPoint(players(2))
      _ <- Game.awardPoint(players(2))
      ty <- Game.getPlayer(players(0))
      kev <- Game.getPlayer(players(1))
      mo <- Game.getPlayer(players(2))
      trev <- Game.getPlayer(players(3))
    } yield (ty.get, kev.get, mo.get, trev.get)

    val (newGame, (ty, kev, mo, trev)) = giveSomePoints(game)
    newGame.players should contain theSameElementsAs Seq(ty, kev, mo, trev)
    ty.score should be (1)
    kev.score should be (1)
    mo.score should be (2)
    trev.score should be (0)
  }

  it should "not change the state if a point is given to a user not in the game"

  behavior of "The discard pile"

  it should "be empty at the start of the game" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game(players)

    game.discard shouldBe empty
  }

  it should "contain cards that players discard" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game(players)

    def shuffleDrawAndDiscard(r: Random) = for {
      _ <- Game.shuffle(r)
      player <- Game.drawCard(players.head)
      discard <- Game.playerDiscard(players.head, player.get.hand.head)
    } yield (player, discard)

    val (newGame, (player, discard)) = shuffleDrawAndDiscard(new Random(1345))(game)
    newGame.discard should be (discard)
    discard.head should be (player.get.hand.head)
  }

  it should "remain the same if a player that doesn't exist tries to discard a card" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game(players)

    def drawAndDiscardInvalidCard = for {
      player <- Game.drawCard(players(0))
      discard <- Game.playerDiscard("BADPLAYER", org.romeo.loveletter.game.Princess)
    } yield (player, discard)

    val (player, discard) = drawAndDiscardInvalidCard.eval(game)
    player.get.hand.head should be (org.romeo.loveletter.game.Guard)
    discard shouldBe empty

  }

  it should "remain the same if a player tries to discard a card they don't have" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game(players)

    def drawAndDiscardInvalidCard = for {
      player <- Game.drawCard(players(0))//should draw a guard
      discard <- Game.playerDiscard(players(0), org.romeo.loveletter.game.Princess)
    } yield (player, discard)

    val (player, discard) = drawAndDiscardInvalidCard.eval(game)
    player.get.hand.head should be (org.romeo.loveletter.game.Guard)
    discard shouldBe empty

  }
}
