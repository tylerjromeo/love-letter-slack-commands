package org.romeo.loveletter.game

import scala.language.postfixOps
import scala.util.Random
import org.scalatest._
import scalaz.State

class EngineSpec extends FlatSpec with Matchers {

  behavior of "A game"

  it should "fail to start with too few players" in {
    val tooFewPlayers = Seq("Tyler")
    an[IllegalArgumentException] should be thrownBy Game(tooFewPlayers)
  }

  it should "fail to start with too many players" in {
    val tooManyPlayers = Seq("Tyler", "Kevin", "Morgan", "Trevor", "Jeff")
    an[IllegalArgumentException] should be thrownBy Game(tooManyPlayers)
  }

  it should "report a winner if a player has 7 points in a 2 player game" in {
    val players = Seq("Tyler", "Kevin")
    val game = Game(players)

    def giveSomePoints = for {
      _ <- Game.awardPoint(players(1))
      _ <- Game.awardPoint(players(1))
      _ <- Game.awardPoint(players(1))
      _ <- Game.awardPoint(players(1))
      _ <- Game.awardPoint(players(1))
      _ <- Game.awardPoint(players(1))
      nonWinner <- Game.findWinner
      _ <- Game.awardPoint(players(1))
      winner <- Game.findWinner
    } yield (nonWinner, winner)

    val (nonWinner, winner) = giveSomePoints.eval(game)
    nonWinner should be(empty)
    winner.get.name should be(players(1))
    winner.get.score should be(7)
  }

  it should "report a winner if a player has 5 points in a 3 player game" in {
    val players = Seq("Tyler", "Kevin", "Morgan")
    val game = Game(players)

    def giveSomePoints = for {
      _ <- Game.awardPoint(players(1))
      _ <- Game.awardPoint(players(1))
      _ <- Game.awardPoint(players(1))
      _ <- Game.awardPoint(players(1))
      nonWinner <- Game.findWinner
      _ <- Game.awardPoint(players(1))
      winner <- Game.findWinner
    } yield (nonWinner, winner)

    val (nonWinner, winner) = giveSomePoints.eval(game)
    nonWinner should be(empty)
    winner.get.name should be(players(1))
    winner.get.score should be(5)
  }

  it should "report a winner if a player has 4 points in a 4 player game" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game(players)

    def giveSomePoints = for {
      _ <- Game.awardPoint(players(1))
      _ <- Game.awardPoint(players(1))
      _ <- Game.awardPoint(players(1))
      nonWinner <- Game.findWinner
      _ <- Game.awardPoint(players(1))
      winner <- Game.findWinner
    } yield (nonWinner, winner)

    val (nonWinner, winner) = giveSomePoints.eval(game)
    nonWinner should be(empty)
    winner.get.name should be(players(1))
    winner.get.score should be(4)
  }

  behavior of "The first player"

  it should "be the current player" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game(players)
    Game.currentPlayer.eval(game).name should be(players.head)
  }

  it should "change when a turn is ended" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game(players)
    Game.endTurn.exec(game).players.head.name should be(players(1))
  }

  it should "be back to its initial state after a full round of turns" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game(players)

    def FullRoundCycle() = for {
      p1 <- Game.currentPlayer
      p2 <- Game.endTurn
      p3 <- Game.endTurn
      p4 <- Game.endTurn
      _ <- Game.endTurn
    } yield Seq(p1, p2, p3, p4)

    //after 4 turns change the state should be back to where it started
    val (newGame, newPlayers) = FullRoundCycle()(game)
    newGame should be(game)
    newPlayers.map(_.name) should be(players)
  }

  behavior of "A match"

  it should "have 2 cards for the current player" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game.startMatch(Some(players(0)))(new Random(9191)).exec(Game(players))

    val currentPlayer = Game.currentPlayer.eval(game)
    currentPlayer.hand should have length 2
  }

  it should "have 1 for player whose turn it isn't when starting a game" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game.startMatch(Some(players(0)))(new Random(9192)).exec(Game(players))

    val currentPlayer = Game.currentPlayer.eval(game)
    (game.players.diff(Seq(currentPlayer))).foreach(_.hand should have length 1)
  }

  it should "have the same number of cards in the game as were in the deck (minus burn card)" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game.startMatch(Some(players(0)))(new Random(9193)).exec(Game(players))

    val cardsInPlay = game.players.foldLeft(0)((acc, p) => acc + p.hand.length) + game.deck.length + game.visibleDiscard.length + game.discard.length
    cardsInPlay + 1 should be(org.romeo.loveletter.game.Deck.cards.length)
  }

  it should "have the same number of cards in the game as were in the deck (minus burn card) for a 2 player game" in {
    val players = Seq("Tyler", "Kevin")
    implicit val r = new Random(9194)
    val game = Game.startMatch(Some(players(0))).exec(Game(players))

    val cardsInPlay = game.players.foldLeft(0)((acc, p) => acc + p.hand.length) + game.deck.length + game.visibleDiscard.length + game.discard.length
    cardsInPlay + 1 should be(org.romeo.loveletter.game.Deck.cards.length)
  }

  it should "have 3 visible discards in a 2 player game" in {
    val players = Seq("Tyler", "Kevin")
    implicit val r = new Random(9195)
    val game = Game.startMatch(Some(players(0))).exec(Game(players))

    game.visibleDiscard should have length (3)
  }

  it should "have no visible discards in a non 2 player game" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    implicit val r = new Random(9196)
    val game = Game.startMatch(Some(players(0))).exec(Game(players))

    game.visibleDiscard should have length (0)
  }

  it should "have all players not eliminated and not protected, even if they were in the previous match" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game(players)

    def modifyPlayersThenRestartMatch = for {
      _ <- Game.startMatch()(new Random(8375))
      _ <- Game.protectPlayer(players(0), true)
      _ <- Game.eliminatePlayer(players(0), true)
      _ <- Game.protectPlayer(players(1), true)
      _ <- Game.eliminatePlayer(players(2), true)
      _ <- Game.startMatch()(new Random(7482))
    } yield ()

    val newGame = modifyPlayersThenRestartMatch.exec(game)
    newGame.players.foreach { p =>
      p.isProtected should be(false)
      p.isEliminated should be(false)
    }
  }

  it should "have no winner if more than one player is left and the discard is not empty" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game(players)
    implicit val r = new Random(795)

    def repeat[A, B](n: Int, s: State[A, B]): State[A, B] = if (n <= 1) s else s.flatMap(_ => repeat(n - 1, s))

    def playSome = for {
      _ <- Game.startMatch(Some(players(0)))
      _ <- repeat(3, Game.drawCard(players(0)))
      _ <- Game.eliminatePlayer(players(2), true)
      winner <- Game.checkMatchOver(new Random(7374))
    } yield winner

    val (newGame, winner) = playSome(game)
    //if the game hasn't restarted, the deck should have subtracted a burn card,
    //a card per player, the first players initial draw, and the 3 extras we drew
    newGame.deck.length should be(org.romeo.loveletter.game.Deck.cards.size - 1 - players.size - 1 - 3)
    winner should be(empty)
  }

  it should "restart the match if there is a winner" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game(players)

    def eliminateEveryoneElse = for {
      _ <- Game.startMatch(Some(players(0)))(new Random(9294))
      _ <- Game.eliminatePlayer(players(0), true)
      _ <- Game.eliminatePlayer(players(1), true)
      _ <- Game.eliminatePlayer(players(2), true)
      winner <- Game.checkMatchOver(new Random(85930))
    } yield winner

    val newGame = eliminateEveryoneElse.exec(game)
    newGame.deck should not be (game.deck)
    newGame.players.foreach(_.isEliminated should be(false))
  }

  it should "have the first player of the next match be the winner of the last match" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game(players)

    def eliminateEveryoneElseThenStartNewMatch = for {
      _ <- Game.startMatch(Some(players(0)))(new Random(9294))
      _ <- Game.eliminatePlayer(players(0), true)
      _ <- Game.eliminatePlayer(players(1), true)
      _ <- Game.eliminatePlayer(players(2), true)
      winner <- Game.checkMatchOver(new Random(85930))
      firstPlayer <- Game.currentPlayer
    } yield (winner, firstPlayer)

    val (winner, firstPlayer) = eliminateEveryoneElseThenStartNewMatch.eval(game)

    firstPlayer.name should be(winner.get.name)
  }

  it should "have the last remaining player be detected as the winner" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game(players)

    def eliminateEveryoneElse = for {
      _ <- Game.startMatch(Some(players(0)))(new Random(9294))
      _ <- Game.eliminatePlayer(players(0), true)
      _ <- Game.eliminatePlayer(players(1), true)
      _ <- Game.eliminatePlayer(players(2), true)
      winner <- Game.checkMatchOver(new Random(85930))
    } yield winner

    val winner = eliminateEveryoneElse.eval(game)
    winner.get.name should be(players(3))
  }

  it should "if the deck is empty, have the non eliminated player with the highest card be detected as the winner" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game(players)

    def repeat[A, B](n: Int, s: State[A, B]): State[A, B] = if (n <= 1) s else s.flatMap(_ => repeat(n - 1, s))

    val singleTurn = for {
      _ <- Game.endTurn
      currentPlayer <- Game.currentPlayer
      _ <- Game.drawCard(currentPlayer.name)
    } yield ()

    val playTheGame = for {
      _ <- Game.startMatch(Some(players(0)))(new Random(3)) //with a seed of 3, player(0) should get the 8, and player(2) should get the 7
      _ <- repeat(org.romeo.loveletter.game.Deck.cards.length - 6, singleTurn)
      _ <- Game.eliminatePlayer(players(0), true)
      winner <- Game.checkMatchOver(new Random(4131))
    } yield winner

    val (newGame, winner) = playTheGame(game)

    winner should not be (empty)
    winner.get.name should be(players(2))
    Game.getPlayer(players(2)).eval(newGame).map(_.score).getOrElse(2) should be(1)
  }

  it should "if the deck is empty, have the player with the highest card be detected as the winner" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game(players)

    def repeat[A, B](n: Int, s: State[A, B]): State[A, B] = if (n <= 1) s else s.flatMap(_ => repeat(n - 1, s))

    val singleTurn = for {
      _ <- Game.endTurn
      currentPlayer <- Game.currentPlayer
      _ <- Game.drawCard(currentPlayer.name)
    } yield ()

    val playTheGame = for {
      _ <- Game.startMatch(Some(players(0)))(new Random(3)) //with a seed of 3, player(0) should get the 8
      _ <- repeat(org.romeo.loveletter.game.Deck.cards.length - 6, singleTurn)
      winner <- Game.checkMatchOver(new Random(4131))
    } yield winner

    val (newGame, winner) = playTheGame(game)

    winner should not be (empty)
    winner.get.name should be(players(0))
    Game.getPlayer(players(0)).eval(newGame).map(_.score).getOrElse(0) should be(1)
  }

  behavior of "The game deck"

  it should "be put in random order when shuffled" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game(players)
    val r1 = new Random(1337)
    val r2 = new Random(1337)

    val (newGame, shuffledDeck) = Game.shuffle(r1)(game)
    newGame.deck should be(shuffledDeck)
    shuffledDeck should not be (Deck.cards)
    shuffledDeck should contain theSameElementsAs Deck.cards
    shuffledDeck should be(r2.shuffle(Deck.cards))
  }

  it should "lose its top card when burnCard is called" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game(players)

    def shuffleAndBurn(r: Random) = for {
      deck <- Game.shuffle(r)
      burntCard <- Game.burnCard
    } yield (deck, burntCard)

    val (newGame, (newDeck, burntCard)) = shuffleAndBurn(new Random(1338))(game)
    newGame.deck should be(newDeck.tail)
    newGame.discard should be(game.discard)
    newGame.visibleDiscard should be(game.visibleDiscard)
  }

  it should "return the burned card when burncard is called" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game(players)

    def shuffleAndBurn(r: Random) = for {
      deck <- Game.shuffle(r)
      burntCard <- Game.burnCard
    } yield (deck, burntCard)

    val (newGame, (newDeck, burntCard)) = shuffleAndBurn(new Random(1339))(game)
    Seq(burntCard) ++ newGame.deck should be(newDeck)
  }

  it should "not have any change in the discard piles after burning" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game(players)

    def shuffleAndBurn(r: Random) = for {
      deck <- Game.shuffle(r)
      burntCard <- Game.burnCard
    } yield (deck, burntCard)

    val newGame = shuffleAndBurn(new Random(1340)).exec(game)
    newGame.discard should be(game.discard)
    newGame.visibleDiscard should be(game.visibleDiscard)
  }

  it should "show the burned card if burnCardVisible is called" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game(players)

    def shuffleAndBurn(r: Random) = for {
      _ <- Game.shuffle(r)
      burntCard <- Game.burnCardVisible
    } yield burntCard

    val (newGame, burntCard) = shuffleAndBurn(new Random(1341))(game)
    newGame.discard should be(game.discard)
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
    newGame.deck should be(startingDeck.tail)
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
    newGame.players(0) should be(newPlayer.get)
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

    game.players.foreach(_.score should be(0))
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
    ty.score should be(1)
    kev.score should be(1)
    mo.score should be(2)
    trev.score should be(0)
  }

  it should "not change the state if a point is given to a user not in the game" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game(players)

    val newGame = Game.awardPoint("BADPLAYER").exec(game)
    newGame should be(game)
  }

  it should "be eliminated if eliminate is called on them" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game(players)

    def eliminateSome = for {
      ty <- Game.eliminatePlayer(players(0), true)
      kev <- Game.eliminatePlayer(players(1), true)
    } yield (ty.get, kev.get)

    val (newGame, (ty, kev)) = eliminateSome(game)
    ty.isEliminated should be(true)
    kev.isEliminated should be(true)
    Game.getPlayer(players(0)).eval(newGame).get.isEliminated should be(true)
    Game.getPlayer(players(1)).eval(newGame).get.isEliminated should be(true)
    Game.getPlayer(players(2)).eval(newGame).get.isEliminated should be(false)
    Game.getPlayer(players(3)).eval(newGame).get.isEliminated should be(false)
  }

  it should "have their card discarded if they are eliminated" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    implicit val r = new Random(73705)
    val game = Game.startMatch(Some(players(0))).exec(Game(players))

    def checkKevinsCardThenEliminate = for {
      kev <- Game.getPlayer(players(1))
      _ <- Game.eliminatePlayer(players(1), true)
    } yield kev.get.hand.head

    val (newGame, kevsCard) = checkKevinsCardThenEliminate(game)

    newGame.discard.head should be(kevsCard)
  }

  it should "not change the state of the game if a user not in the game is eliminated" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game(players)

    Game.eliminatePlayer("BADPLAYER", true).exec(game) should be(game)
  }

  it should "be protected if protect is called on them" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game(players)

    def protectSome = for {
      ty <- Game.protectPlayer(players(0), true)
      kev <- Game.protectPlayer(players(1), true)
    } yield (ty.get, kev.get)

    val (newGame, (ty, kev)) = protectSome(game)
    ty.isProtected should be(true)
    kev.isProtected should be(true)
    Game.getPlayer(players(0)).eval(newGame).get.isProtected should be(true)
    Game.getPlayer(players(1)).eval(newGame).get.isProtected should be(true)
    Game.getPlayer(players(2)).eval(newGame).get.isProtected should be(false)
    Game.getPlayer(players(3)).eval(newGame).get.isProtected should be(false)
  }

  it should "no longer be protected one their turn starts" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    implicit val r = new Random(24)
    //Use 24 as a seed for tests that need to ignore card effects. This will give the first player a countess
    val game = Game.startMatch(Some(players(0))).exec(Game(players))

    def protectPlayerThenStartTheirTurn = for {
      p1 <- Game.currentPlayer
      p2 <- Game.protectPlayer(players(1), true)
      _ <- Game.processTurn(p1.name, p1.hand.head)
      p3 <- Game.getPlayer(players(1))
    } yield (p2.get, p3.get)

    val (protectedPlayer, unprotectedPlayer) = protectPlayerThenStartTheirTurn.eval(game)

    protectedPlayer.isProtected should be(true)
    unprotectedPlayer.isProtected should be(false)
  }

  it should "be skipped in the turn order if they are eliminated" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    implicit val r = new Random(24)
    //Use 24 as a seed for tests that need to ignore card effects. This will give the first player a countess
    val game = Game.startMatch(Some(players(0))).exec(Game(players))

    def eliminateThenSwitchTurns = for {
      _ <- Game.eliminatePlayer(players(1), true)
      p1 <- Game.currentPlayer
      _ <- Game.processTurn(p1.name, p1.hand.head)
      p2 <- Game.currentPlayer
    } yield p2

    val nextPlayer = eliminateThenSwitchTurns.eval(game)
    nextPlayer.name should be(players(2))
  }

  it should "not change the state of the game if a user not in the game is protected" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game(players)

    Game.protectPlayer("BADPLAYER", true).exec(game) should be(game)
  }

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
    newGame.discard should be(discard)
    discard.head should be(player.get.hand.head)
  }

  it should "remain the same if a player that doesn't exist tries to discard a card" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game(players)

    def drawAndDiscardInvalidCard = for {
      player <- Game.drawCard(players(0))
      discard <- Game.playerDiscard("BADPLAYER", org.romeo.loveletter.game.Princess)
    } yield (player, discard)

    val (player, discard) = drawAndDiscardInvalidCard.eval(game)
    player.get.hand.head should be(org.romeo.loveletter.game.Guard)
    discard shouldBe empty

  }

  it should "remain the same if a player tries to discard a card they don't have" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game(players)

    def drawAndDiscardInvalidCard = for {
      player <- Game.drawCard(players(0)) //should draw a guard
      discard <- Game.playerDiscard(players(0), org.romeo.loveletter.game.Princess)
    } yield (player, discard)

    val (player, discard) = drawAndDiscardInvalidCard.eval(game)
    player.get.hand.head should be(org.romeo.loveletter.game.Guard)
    discard shouldBe empty

  }

  behavior of "taking a turn"

  it should "not change the game if a player not in the game tries to go" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    implicit val r = new Random(761247)
    val game = Game.startMatch(Some(players(0))).exec(Game(players))

    val newGame = Game.processTurn("BADPLAYER", Countess).exec(game)

    newGame should be(game)
  }

  it should "not change the game if it's not the player's turn" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    implicit val r = new Random(3)
    //a seed of 3 gives the 4th player a countess, so there will be no side effects from the discard
    val game = Game.startMatch(Some(players(0))).exec(Game(players))

    val newGame = (for {
      p <- Game.getPlayer(players(3))
      _ <- Game.processTurn(players(3), p.get.hand.head)
    } yield ()).exec(game)

    newGame should be(game)
  }

  it should "not change the game if the player doesn't have the cards he wants to discard" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    implicit val r = new Random(81234578)
    //won't give the player a countess
    val game = Game.startMatch(Some(players(0))).exec(Game(players))

    val newGame = (for {
      p <- Game.currentPlayer
      _ <- Game.processTurn(p.name, Countess)
    } yield ()).exec(game)

    newGame should be(game)
  }

  it should "be the next player's turn after the current player takes a turn" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    implicit val r = new Random(9195)
    val game = Game.startMatch(Some(players(0))).exec(Game(players))

    def takeATurnThenGetNextPlayer = for {
      p <- Game.currentPlayer
      _ <- Game.processTurn(p.name, p.hand.head)
      p2 <- Game.currentPlayer
    } yield p2

    val (newGame, nextPlayer) = takeATurnThenGetNextPlayer(game)

    nextPlayer.name should be(players(1))
  }

  it should "cause the next player to draw a card after the current player takes a turn" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    implicit val r = new Random(7373)
    val game = Game.startMatch(Some(players(0))).exec(Game(players))

    def takeATurnThenGetNextPlayer = for {
      p <- Game.currentPlayer
      _ <- Game.processTurn(p.name, p.hand.head)
      p2 <- Game.currentPlayer
    } yield p2

    val (newGame, nextPlayer) = takeATurnThenGetNextPlayer(game)

    nextPlayer.name should be(players(1))
    nextPlayer.hand should have length (2)
  }

  it should "add the discarded card to the discard pile" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    implicit val r = new Random(24)
    //Use 24 as a seed for tests that need to ignore card effects. This will give the first player a countess
    val game = Game.startMatch(Some(players(0))).exec(Game(players))

    def takeATurn = for {
      p <- Game.currentPlayer
      _ <- Game.processTurn(p.name, p.hand.head)
    } yield p.hand.head

    val (newGame, discard) = takeATurn(game)

    newGame.discard.head should be(discard)
  }

  it should "detect and return the winner of a match" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    implicit val r = new Random(5672)
    val game = Game.startMatch(Some(players(0))).exec(Game(players))

    def makeSomeoneWinnerThenTakeTurn = for {
      _ <- Game.eliminatePlayer(players(1), true)
      _ <- Game.eliminatePlayer(players(2), true)
      _ <- Game.eliminatePlayer(players(3), true)
      p <- Game.currentPlayer
      winners <- Game.processTurn(p.name, p.hand.head)
    } yield winners

    val winners = makeSomeoneWinnerThenTakeTurn.eval(game)

    winners.isRight should be(true)
    winners.right.get.map(_.msg) should contain(s"${players(0)} has won the match!")
  }

  it should "start a new match if there is a winner" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val seed = 93713
    //Use 24 as a seed for tests that need to ignore card effects. This will give the first player a countess
    val game = Game.startMatch(Some(players(0)))(new Random(24)).exec(Game(players))

    def makeSomeoneWinner = for {
      _ <- Game.eliminatePlayer(players(1), true)
      _ <- Game.eliminatePlayer(players(2), true)
      _ <- Game.eliminatePlayer(players(3), true)
    } yield ()

    val newGame1 = makeSomeoneWinner.exec(game)
    val newGame2 = (for {
      p <- Game.currentPlayer
      _ <- Game.processTurn(p.name, p.hand.head)(new Random(seed))
    } yield ()).exec(newGame1)

    newGame1.deck should not be newGame2.deck
    newGame2.players.foreach(_.isEliminated should be(false))
    //To prove that a new match has started, shuffle a new deck with the same seed as the new match.
    //If the decks have the same tails, that shows that there's a new game
    Game.shuffle(new Random(seed)).exec(Game(players)).deck.endsWith(newGame2.deck) should be(true)

  }

  it should "detect and return the winner of a game" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    implicit val r = new Random(24)
    //Use 24 as a seed for tests that need to ignore card effects. This will give the first player a countess
    val game = Game.startMatch(Some(players(0))).exec(Game(players))

    def makeSomeoneWinnerThenTakeTurn = for {
      _ <- Game.eliminatePlayer(players(1), true)
      _ <- Game.eliminatePlayer(players(2), true)
      _ <- Game.eliminatePlayer(players(3), true)
      _ <- Game.awardPoint(players(0))
      _ <- Game.awardPoint(players(0))
      _ <- Game.awardPoint(players(0))
      p <- Game.currentPlayer
      winners <- Game.processTurn(p.name, p.hand.head)
    } yield winners

    val winners = makeSomeoneWinnerThenTakeTurn.eval(game)

    winners.isRight should be(true)
    winners.right.get.last.msg should be(s"${players(0)} has won the game!")
  }

  behavior of "The guard card"

  it should "eliminate a player if their card is guessed" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    implicit val r = new Random(11)
    //seed 11 gives player 1 a guard, and player 2 a priest
    val game = Game.startMatch(Some(players(0))).exec(Game(players))

    def playGuardAndGuessRight = for {
      p <- Game.currentPlayer
      result <- Game.processTurn(p.name, Guard, Some(players(1)), Some(Priest))
      target <- Game.getPlayer(players(1))
    } yield (result, target.get)

    val (newGame, (message, target)) = playGuardAndGuessRight(game)

    message.isRight should be(true)
    target.isEliminated should be(true)
  }

  it should "not eliminate a player if their card is guessed incorrectly" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    implicit val r = new Random(11)
    //seed 11 gives player 1 a guard, and player 2 a priest
    val game = Game.startMatch(Some(players(0))).exec(Game(players))

    def playGuardAndGuessRight = for {
      p <- Game.currentPlayer
      result <- Game.processTurn(p.name, Guard, Some(players(1)), Some(Baron)) //WRONG
      target <- Game.getPlayer(players(1))
    } yield (result, target.get)

    val (newGame, (message, target)) = playGuardAndGuessRight(game)

    message.isRight should be(true)
    target.isEliminated should be(false)
  }

  it should "fail if guard is guessed" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    implicit val r = new Random(11)
    //seed 11 gives player 1 a guard, and player 2 a priest
    val game = Game.startMatch(Some(players(0))).exec(Game(players))

    def playGuardAndGuessRight = for {
      p <- Game.currentPlayer
      result <- Game.processTurn(p.name, Guard, Some(players(1)), Some(Guard))
      currentPlayer <- Game.currentPlayer
    } yield (result, currentPlayer)

    val (newGame, (message, currentPlayer)) = playGuardAndGuessRight(game)

    message.isLeft should be(true)
    currentPlayer.name should be(players(0))
  }

  it should "fail if a player not in the game is targeted" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    implicit val r = new Random(11)
    //seed 11 gives player 1 a guard, and player 2 a priest
    val game = Game.startMatch(Some(players(0))).exec(Game(players))

    def playGuardAndGuessBadplayer = for {
      p <- Game.currentPlayer
      result <- Game.processTurn(p.name, Guard, Some("BADPLAYER"), Some(Priest))
      currentPlayer <- Game.currentPlayer
    } yield (result, currentPlayer)

    val (newGame, (message, currentPlayer)) = playGuardAndGuessBadplayer(game)

    message.isLeft should be(true)
    currentPlayer.name should be(players(0))
  }

  it should "fail if the targeted player is protected" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    implicit val r = new Random(11)
    //seed 11 gives player 1 a guard, and player 2 a priest
    val game = Game.startMatch(Some(players(0))).exec(Game(players))

    def protectThenPlayGuard = for {
      _ <- Game.protectPlayer(players(1), true)
      p <- Game.currentPlayer
      result <- Game.processTurn(p.name, Guard, Some(players(1)), Some(Priest))
      currentPlayer <- Game.currentPlayer
    } yield (result, currentPlayer)

    val (message, currentPlayer) = protectThenPlayGuard.eval(game)

    message.isLeft should be(true)
    currentPlayer.name should be(players(0))
  }

  it should "fail if the targeted player is eliminated" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    implicit val r = new Random(11)
    //seed 11 gives player 1 a guard, and player 2 a priest
    val game = Game.startMatch(Some(players(0))).exec(Game(players))

    def protectThenPlayGuard = for {
      _ <- Game.eliminatePlayer(players(1), true)
      p <- Game.currentPlayer
      result <- Game.processTurn(p.name, Guard, Some(players(1)), Some(Priest))
      currentPlayer <- Game.currentPlayer
    } yield (result, currentPlayer)

    val (message, currentPlayer) = protectThenPlayGuard.eval(game)

    message.isLeft should be(true)
    currentPlayer.name should be(players(0))
  }

  it should "allow the player to play with no effect if every other player is eliminated or protected" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    implicit val r = new Random(11)
    //seed 11 gives player 1 a guard, and player 2 a priest
    val game = Game.startMatch(Some(players(0))).exec(Game(players))

    def protectOrEliminateEveryoneThenPlay = for {
      _ <- Game.protectPlayer(players(1), true)
      _ <- Game.eliminatePlayer(players(2), true)
      _ <- Game.protectPlayer(players(3), true)
      result <- Game.processTurn(players(0), Guard, None, None)
      p <- Game.currentPlayer
    } yield (p, result)

    val (nextPlayer, result) = protectOrEliminateEveryoneThenPlay.eval(game)
    result.isRight should be(true)
    nextPlayer.name should be(players(1))

  }

  it should "fail if a target or a guess is not supplied" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    implicit val r = new Random(432345)
    val game = Game(players)
    val (game1, result1) = Game.processTurn(players(0), Guard, None, Some(Priest)).apply(game)
    game1 should be(game)
    result1.isLeft should be(true)

    val (game2, result2) = Game.processTurn(players(0), Guard, Some(players(2)), None).apply(game)
    game2 should be(game)
    result2.isLeft should be(true)
  }

  behavior of "The priest card"

  it should "show the card of the targeted player in the returned message" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    implicit val r = new Random(1)
    //seed 1 gives player 1 a priest, and player 2 a guard
    val game = Game.startMatch(Some(players(0))).exec(Game(players))

    def peekAtPlayersCard = for {
      p <- Game.currentPlayer
      result <- Game.processTurn(p.name, Priest, Some(players(1)), None)
    } yield result

    val message = peekAtPlayersCard.eval(game)

    message.isRight should be(true)
    message.right.get.head.msg.contains(Guard.name) should be(true)
  }

  it should "fail if a player not in the game is targeted" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    implicit val r = new Random(1)
    //seed 1 gives player 1 a priest, and player 2 a guard
    val game = Game.startMatch(Some(players(0))).exec(Game(players))

    def playPriestOnBadplayer = for {
      p <- Game.currentPlayer
      result <- Game.processTurn(p.name, Priest, Some("BADPLAYER"), None)
      currentPlayer <- Game.currentPlayer
    } yield (result, currentPlayer)

    val (newGame, (message, currentPlayer)) = playPriestOnBadplayer(game)

    message.isLeft should be(true)
    currentPlayer.name should be(players(0))
  }

  it should "fail if the targeted player is protected" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    implicit val r = new Random(1)
    //seed 1 gives player 1 a priest, and player 2 a guard
    val game = Game.startMatch(Some(players(0))).exec(Game(players))

    def playPriestOnProtectedPlayer = for {
      _ <- Game.protectPlayer(players(1), true)
      p <- Game.currentPlayer
      result <- Game.processTurn(p.name, Priest, Some(players(1)), None)
      currentPlayer <- Game.currentPlayer
    } yield (result, currentPlayer)

    val (newGame, (message, currentPlayer)) = playPriestOnProtectedPlayer(game)

    message.isLeft should be(true)
    currentPlayer.name should be(players(0))
  }

  it should "fail if the targeted player is eliminated" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    implicit val r = new Random(1)
    //seed 1 gives player 1 a priest, and player 2 a guard
    val game = Game.startMatch(Some(players(0))).exec(Game(players))

    def playPriestOnEliminatedPlayer = for {
      _ <- Game.eliminatePlayer(players(1), true)
      p <- Game.currentPlayer
      result <- Game.processTurn(p.name, Priest, Some(players(1)), None)
      currentPlayer <- Game.currentPlayer
    } yield (result, currentPlayer)

    val (newGame, (message, currentPlayer)) = playPriestOnEliminatedPlayer(game)

    message.isLeft should be(true)
    currentPlayer.name should be(players(0))
  }

  it should "allow the player to play with no effect if every other player is eliminated or protected" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    implicit val r = new Random(1)
    //seed 1 gives player 1 a priest, and player 2 a guard
    val game = Game.startMatch(Some(players(0))).exec(Game(players))

    def protectOrEliminateEveryoneThenPlay = for {
      _ <- Game.protectPlayer(players(1), true)
      _ <- Game.eliminatePlayer(players(2), true)
      _ <- Game.protectPlayer(players(3), true)
      result <- Game.processTurn(players(0), Priest, None, None)
      p <- Game.currentPlayer
    } yield (p, result)

    val (nextPlayer, result) = protectOrEliminateEveryoneThenPlay.eval(game)
    result.isRight should be(true)
    nextPlayer.name should be(players(1))

  }

  it should "fail if a target is not supplied" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    implicit val r = new Random(432345)
    val game = Game(players)
    val (game1, result1) = Game.processTurn(players(0), Priest, None, None).apply(game)
    game1 should be(game)
    result1.isLeft should be(true)
  }

  behavior of "The baron card"

  it should "eliminate the target player if your card is higher than theirs" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    implicit val r = new Random(12)
    //seed 0 gives player 1 a baron and a countess, and player 2 a guard
    val game = Game.startMatch(Some(players(0))).exec(Game(players))

    val (newGame, response) = Game.processTurn(players(0), Baron, Some(players(1)), None).apply(game)

    response.isRight should be(true)
    Game.getPlayer(players(0)).eval(newGame).get.isEliminated should be(false)
    Game.getPlayer(players(1)).eval(newGame).get.isEliminated should be(true)
    Game.currentPlayer.eval(newGame).name should be(players(2))

  }

  it should "eliminate you if the target players card is higher than yours" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    implicit val r = new Random(0)
    //seed 0 gives player 1 a baron and a priest, and player 2 a prince
    val game = Game.startMatch(Some(players(0))).exec(Game(players))

    val (newGame, response) = Game.processTurn(players(0), Baron, Some(players(1)), None).apply(game)

    response.isRight should be(true)
    Game.getPlayer(players(0)).eval(newGame).get.isEliminated should be(true)
    Game.getPlayer(players(1)).eval(newGame).get.isEliminated should be(false)
    Game.currentPlayer.eval(newGame).name should be(players(1))

  }

  it should "eliminate nobody if you have the same card" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    implicit val r = new Random(23)
    //seed 0 gives player 1 a baron and a handmaid, and player 2 a handmaid
    val game = Game.startMatch(Some(players(0))).exec(Game(players))

    val (newGame, response) = Game.processTurn(players(0), Baron, Some(players(1)), None).apply(game)

    response.isRight should be(true)
    Game.getPlayer(players(0)).eval(newGame).get.isEliminated should be(false)
    Game.getPlayer(players(1)).eval(newGame).get.isEliminated should be(false)
    Game.currentPlayer.eval(newGame).name should be(players(1))

  }

  it should "fail if a player not in the game is targeted" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    implicit val r = new Random(0)
    //seed 0 gives player 1 a baron, and player 2 a prince
    val game = Game.startMatch(Some(players(0))).exec(Game(players))

    def playBaronOnBadPlayer = for {
      p <- Game.currentPlayer
      result <- Game.processTurn(p.name, Baron, Some("BADPLAYER"), None)
      currentPlayer <- Game.currentPlayer
    } yield (result, currentPlayer)

    val (newGame, (message, currentPlayer)) = playBaronOnBadPlayer(game)

    message.isLeft should be(true)
    currentPlayer.name should be(players(0))
  }

  it should "fail if the targeted player is protected" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    implicit val r = new Random(0)
    //seed 0 gives player 1 a baron, and player 2 a prince
    val game = Game.startMatch(Some(players(0))).exec(Game(players))

    def playBaronOnProtectedPlayer = for {
      _ <- Game.protectPlayer(players(1), true)
      p <- Game.currentPlayer
      result <- Game.processTurn(p.name, Baron, Some(players(1)), None)
      currentPlayer <- Game.currentPlayer
    } yield (result, currentPlayer)

    val (newGame, (message, currentPlayer)) = playBaronOnProtectedPlayer(game)

    message.isLeft should be(true)
    currentPlayer.name should be(players(0))
  }

  it should "fail if the targeted player is eliminated" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    implicit val r = new Random(0)
    //seed 0 gives player 1 a baron, and player 2 a prince
    val game = Game.startMatch(Some(players(0))).exec(Game(players))

    def playBaronOnEliminatedPlayer = for {
      _ <- Game.eliminatePlayer(players(1), true)
      p <- Game.currentPlayer
      result <- Game.processTurn(p.name, Baron, Some(players(1)), None)
      currentPlayer <- Game.currentPlayer
    } yield (result, currentPlayer)

    val (newGame, (message, currentPlayer)) = playBaronOnEliminatedPlayer(game)

    message.isLeft should be(true)
    currentPlayer.name should be(players(0))
  }

  it should "allow the player to play with no effect if every other player is eliminated or protected" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    implicit val r = new Random(0)
    //seed 0 gives player 1 a baron, and player 2 a prince
    val game = Game.startMatch(Some(players(0))).exec(Game(players))

    def protectOrEliminateEveryoneThenPlay = for {
      _ <- Game.protectPlayer(players(1), true)
      _ <- Game.eliminatePlayer(players(2), true)
      _ <- Game.protectPlayer(players(3), true)
      result <- Game.processTurn(players(0), Baron, None, None)
      p <- Game.currentPlayer
    } yield (p, result)

    val (nextPlayer, result) = protectOrEliminateEveryoneThenPlay.eval(game)
    result.isRight should be(true)
    nextPlayer.name should be(players(1))

  }

  it should "fail if a target is not supplied" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    implicit val r = new Random(432345)
    val game = Game(players)
    val (game1, result1) = Game.processTurn(players(0), Baron, None, None).apply(game)
    game1 should be(game)
    result1.isLeft should be(true)
  }

  behavior of "The handmaid card"

  it should "cause the player to become protected" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    implicit val r = new Random(5)
    //seed 5 gives player 1 a handmaid
    val game = Game.startMatch(Some(players(0))).exec(Game(players))

    def playHandmaid = for {
      p <- Game.currentPlayer
      result <- Game.processTurn(p.name, Handmaid, None, None)
      p1 <- Game.getPlayer(players(0))
    } yield (p1.get, result)

    val (protectedPlayer, message) = playHandmaid.eval(game)

    message.isRight should be(true)
    protectedPlayer.isProtected should be(true)

  }

  behavior of "The prince card"

  it should "cause the targeted player to discard their hand" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    implicit val r = new Random(7)
    //seed 7 gives player 1 a prince, and player 2 a guard
    val game = Game.startMatch(Some(players(0))).exec(Game(players))

    def playPrinceOnPlayer = for {
      p <- Game.currentPlayer
      result <- Game.processTurn(p.name, Prince, Some(players(1)), None)
      currentPlayer <- Game.currentPlayer
    } yield (result, currentPlayer)

    val (newGame, (message, currentPlayer)) = playPrinceOnPlayer(game)

    message.isRight should be(true)
    currentPlayer.name should be(players(1))
    newGame.discard should contain(Prince)
    newGame.discard should contain(Guard)
  }

  it should "cause the player to draw the burn card if they are targeted while there is nothing in the draw pile" in {
    val game = Game(players = Seq(Player(name = "Tyler", hand = Seq(Prince, Prince)), Player(name = "Kevin", hand = Seq(Guard))),
      deck = Nil,
      burnCard = Seq(Princess)
    )

    val (newGame, results) = Game.processTurn("Tyler", Prince, Some("Kevin"), None)(new Random(1423))(game)

    //because the burn card was a Princess, players(1) should have drawn it and won the last round
    results.isRight should be(true)
    results.right.get.map(_.msg) should contain("Kevin has won the match!")
  }

  it should "fail if the targeted player is protected" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    implicit val r = new Random(7)
    //seed 7 gives player 1 a prince, and player 2 a guard
    val game = Game.startMatch(Some(players(0))).exec(Game(players))

    def playPrinceOnProtectedPlayer = for {
      _ <- Game.protectPlayer(players(1), true)
      p <- Game.currentPlayer
      result <- Game.processTurn(p.name, Prince, Some(players(1)), None)
      currentPlayer <- Game.currentPlayer
    } yield (result, currentPlayer)

    val (newGame, (message, currentPlayer)) = playPrinceOnProtectedPlayer(game)

    message.isLeft should be(true)
    currentPlayer.name should be(players(0))
  }

  it should "fail if the targeted player is eliminated" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    implicit val r = new Random(7)
    //seed 7 gives player 1 a prince, and player 2 a guard
    val game = Game.startMatch(Some(players(0))).exec(Game(players))

    def playPrinceOnEliminatedPlayer = for {
      _ <- Game.eliminatePlayer(players(1), true)
      p <- Game.currentPlayer
      result <- Game.processTurn(p.name, Prince, Some(players(1)), None)
      currentPlayer <- Game.currentPlayer
    } yield (result, currentPlayer)

    val (newGame, (message, currentPlayer)) = playPrinceOnEliminatedPlayer(game)

    message.isLeft should be(true)
    currentPlayer.name should be(players(0))
  }

  it should "force the player to target themself if every other player is eliminated or protected" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    implicit val r = new Random(7)
    //seed 7 gives player 1 a prince and a guard
    val game = Game.startMatch(Some(players(0))).exec(Game(players))

    def protectOrEliminateEveryoneThenPlay = for {
      _ <- Game.protectPlayer(players(1), true)
      _ <- Game.protectPlayer(players(2), true)
      _ <- Game.protectPlayer(players(3), true)
      result <- Game.processTurn(players(0), Prince, None, None)
      p2 <- Game.currentPlayer
      result2 <- Game.processTurn(players(0), Prince, Some(players(0)), None)
      p <- Game.currentPlayer
    } yield (p, result, result2, p2)

    val (newGame, (nextPlayer, result, result2, p2)) = protectOrEliminateEveryoneThenPlay(game)
    result.isLeft should be(true)
    result2.isRight should be(true)
    nextPlayer.name should be(players(1))
    newGame.discard should contain only(Guard, Prince)

  }

  it should "fail if a player not in the game is targeted" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    implicit val r = new Random(7)
    //seed 7 gives player 1 a prince
    val game = Game.startMatch(Some(players(0))).exec(Game(players))

    def playPrinceOnBadPlayer = for {
      p <- Game.currentPlayer
      result <- Game.processTurn(p.name, Prince, Some("BADPLAYER"), None)
      currentPlayer <- Game.currentPlayer
    } yield (result, currentPlayer)

    val (newGame, (message, currentPlayer)) = playPrinceOnBadPlayer(game)

    message.isLeft should be(true)
    currentPlayer.name should be(players(0))
  }

  behavior of "The king card"

  it should "switch the remaining card in the current player's hand with the target player" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    implicit val r = new Random(18)
    //seed 18 gives player 1 a King and a handmaid, and player 2 a priest
    val game = Game.startMatch(Some(players(0))).exec(Game(players))

    def playPrinceOnPlayer = for {
      p <- Game.currentPlayer
      result <- Game.processTurn(p.name, King, Some(players(1)), None)
      kingPlayer <- Game.getPlayer(players(0))
      currentPlayer <- Game.currentPlayer
    } yield (result, kingPlayer.get, currentPlayer)

    val (newGame, (message, kingPlayer, currentPlayer)) = playPrinceOnPlayer(game)

    message.isRight should be(true)
    currentPlayer.name should be(players(1))
    currentPlayer.hand should contain(Handmaid)
    kingPlayer.hand should contain only (Priest)
    newGame.discard should contain only (King)
  }

  it should "fail if the targeted player is protected" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    implicit val r = new Random(18)
    //seed 18 gives player 1 a King and a handmaid, and player 2 a priest
    val game = Game.startMatch(Some(players(0))).exec(Game(players))

    def playKingOnProtectedPlayer = for {
      _ <- Game.protectPlayer(players(1), true)
      p <- Game.currentPlayer
      result <- Game.processTurn(p.name, King, Some(players(1)), None)
      currentPlayer <- Game.currentPlayer
    } yield (result, currentPlayer)

    val (newGame, (message, currentPlayer)) = playKingOnProtectedPlayer(game)

    message.isLeft should be(true)
    currentPlayer.name should be(players(0))
  }

  it should "fail if the targeted player is eliminated" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    implicit val r = new Random(18)
    //seed 18 gives player 1 a King and a handmaid, and player 2 a priest
    val game = Game.startMatch(Some(players(0))).exec(Game(players))

    def playKingOnEliminatedPlayer = for {
      _ <- Game.eliminatePlayer(players(1), true)
      p <- Game.currentPlayer
      result <- Game.processTurn(p.name, King, Some(players(1)), None)
      currentPlayer <- Game.currentPlayer
    } yield (result, currentPlayer)

    val (newGame, (message, currentPlayer)) = playKingOnEliminatedPlayer(game)

    message.isLeft should be(true)
    currentPlayer.name should be(players(0))
  }

  it should "allow the player to play with no effect if every other player is eliminated or protected" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    implicit val r = new Random(18)
    //seed 18 gives player 1 a King and a handmaid, and player 2 a priest
    val game = Game.startMatch(Some(players(0))).exec(Game(players))

    def protectOrEliminateEveryoneThenPlay = for {
      _ <- Game.protectPlayer(players(1), true)
      _ <- Game.eliminatePlayer(players(2), true)
      _ <- Game.protectPlayer(players(3), true)
      result <- Game.processTurn(players(0), King, None, None)
      p <- Game.currentPlayer
    } yield (p, result)

    val (nextPlayer, result) = protectOrEliminateEveryoneThenPlay.eval(game)
    result.isRight should be(true)
    nextPlayer.name should be(players(1))

  }

  it should "fail if a player not in the game is targeted" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    implicit val r = new Random(18)
    //seed 18 gives player 1 a King and a handmaid, and player 2 a priest
    val game = Game.startMatch(Some(players(0))).exec(Game(players))

    def playKingOnBadplayer = for {
      p <- Game.currentPlayer
      result <- Game.processTurn(p.name, Priest, Some("BADPLAYER"), None)
      currentPlayer <- Game.currentPlayer
    } yield (result, currentPlayer)

    val (newGame, (message, currentPlayer)) = playKingOnBadplayer(game)

    message.isLeft should be(true)
    currentPlayer.name should be(players(0))
  }

  behavior of "The countess card"

  it should "cause discarding a prince to fail" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    implicit val r = new Random(52)
    //seed 52 gives player 1 a Countess and a Prince
    val game = Game.startMatch(Some(players(0))).exec(Game(players))

    def tryToDiscardPrince = for {
      result <- Game.processTurn(players(0), Prince, Some(players(1)), None)
      p <- Game.currentPlayer
    } yield (p, result)

    val (nextPlayer, result) = tryToDiscardPrince.eval(game)
    result.isLeft should be(true)
    nextPlayer.name should be(players(0))

  }

  it should "cause discarding a king to fail" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    implicit val r = new Random(475)
    //seed 475 gives player 1 a Countess and a King
    val game = Game.startMatch(Some(players(0))).exec(Game(players))

    def tryToDiscardKing = for {
      result <- Game.processTurn(players(0), King, Some(players(1)), None)
      p <- Game.currentPlayer
    } yield (p, result)

    val (nextPlayer, result) = tryToDiscardKing.eval(game)
    result.isLeft should be(true)
    nextPlayer.name should be(players(0))

  }

  it should "have no effect when discarded" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    implicit val r = new Random(24)
    //seed 24 gives player 1 a Countess and a guard
    val game = Game.startMatch(Some(players(0))).exec(Game(players))

    def discardCountess = for {
      result <- Game.processTurn(players(0), Countess, None, None)
      p <- Game.currentPlayer
    } yield (p, result)

    val (nextPlayer, result) = discardCountess.eval(game)
    result.isRight should be(true)
    nextPlayer.name should be(players(1))

  }

  behavior of "The princess card"

  it should "eliminate the player when discarded" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    implicit val r = new Random(3)
    //seed 3 gives player 1 a Princess
    val game = Game.startMatch(Some(players(0))).exec(Game(players))

    def discardPrincess = for {
      result <- Game.processTurn(players(0), Princess, None, None)
      p <- Game.currentPlayer
      princessPlayer <- Game.getPlayer(players(0))
    } yield (p, princessPlayer.get, result)

    val (nextPlayer, princessPlayer, result) = discardPrincess.eval(game)
    result.isRight should be(true)
    nextPlayer.name should be(players(1))
    princessPlayer.isEliminated should be(true)
  }

  it should "eliminate the player forced to discard it by the prince" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    implicit val r = new Random(744)
    //seed 744 gives player 1 a prince and player 2 a Princess
    val game = Game.startMatch(Some(players(0))).exec(Game(players))

    def forceDiscardOfPrincess = for {
      result <- Game.processTurn(players(0), Prince, Some(players(1)), None)
      p <- Game.currentPlayer
      eliminatedPlayer <- Game.getPlayer(players(1))
    } yield (p, eliminatedPlayer.get, result)

    val (nextPlayer, eliminatedPlayer, result) = forceDiscardOfPrincess.eval(game)
    result.isRight should be(true)
    nextPlayer.name should be(players(2))
    eliminatedPlayer.isEliminated should be(true)
  }

  it should "eliminate the player when they are forced to discard by their own prince" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    implicit val r = new Random(15)
    //seed 15 gives player 1 a prince and a Princess
    val game = Game.startMatch(Some(players(0))).exec(Game(players))

    def forceDiscardofPrincess = for {
      _ <- Game.protectPlayer(players(1), true)
      _ <- Game.protectPlayer(players(2), true)
      _ <- Game.protectPlayer(players(3), true)
      result <- Game.processTurn(players(0), Prince, Some(players(0)), None)
      p <- Game.currentPlayer
      eliminatedPlayer <- Game.getPlayer(players(0))
    } yield (p, eliminatedPlayer.get, result)

    val (nextPlayer, eliminatedPlayer, result) = forceDiscardofPrincess.eval(game)
    result.isRight should be(true)
    nextPlayer.name should be(players(1))
    eliminatedPlayer.isEliminated should be(true)
  }
}