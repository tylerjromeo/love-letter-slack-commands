package org.romeo.loveletter.game

import org.romeo.loveletter.game.Game.{GameOver, MatchOver, NextTurn, PlayError}
import org.scalatest.Inside._
import org.scalatest._

import scala.language.postfixOps
import scalaz.State

class EngineSpec extends FlatSpec with Matchers {

  // if the test doesn't need a specific deck or player order, just use this
  val basicRandomizer = Randomizer(
    shuffleDeck = (s: Seq[Card]) => s,
    choosePlayer = (s: Seq[Player]) => s.head
  )

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
    val game = Game.startMatch(Some(players.head))(basicRandomizer).exec(Game(players))

    val currentPlayer = Game.currentPlayer.eval(game)
    currentPlayer.hand should have length 2
  }

  it should "have 1 for player whose turn it isn't when starting a game" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game.startMatch(Some(players.head))(basicRandomizer).exec(Game(players))

    val currentPlayer = Game.currentPlayer.eval(game)
    game.players.diff(Seq(currentPlayer)).foreach(_.hand should have length 1)
  }

  it should "have the same number of cards in the game as were in the deck (minus burn card)" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game.startMatch(Some(players.head))(basicRandomizer).exec(Game(players))

    val cardsInPlay = game.players.foldLeft(0)((acc, p) => acc + p.hand.length) + game.deck.length + game.visibleDiscard.length + game.discard.length
    cardsInPlay + 1 should be(org.romeo.loveletter.game.Deck.cards.length)
  }

  it should "have the same number of cards in the game as were in the deck (minus burn card) for a 2 player game" in {
    val players = Seq("Tyler", "Kevin")
    val game = Game.startMatch(Some(players.head))(basicRandomizer).exec(Game(players))

    val cardsInPlay = game.players.foldLeft(0)((acc, p) => acc + p.hand.length) + game.deck.length + game.visibleDiscard.length + game.discard.length
    cardsInPlay + 1 should be(org.romeo.loveletter.game.Deck.cards.length)
  }

  it should "have 3 visible discards in a 2 player game" in {
    val players = Seq("Tyler", "Kevin")
    val game = Game.startMatch(Some(players.head))(basicRandomizer).exec(Game(players))

    game.visibleDiscard should have length 3
  }

  it should "have no visible discards in a non 2 player game" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game.startMatch(Some(players.head))(basicRandomizer).exec(Game(players))

    game.visibleDiscard should have length 0
  }

  it should "have all players not eliminated and not protected, even if they were in the previous match" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game(players)

    def modifyPlayersThenRestartMatch = for {
      _ <- Game.startMatch()(basicRandomizer)
      _ <- Game.protectPlayer(players.head, isProtected = true)
      _ <- Game.eliminatePlayer(players.head, isEliminated = true)
      _ <- Game.protectPlayer(players(1), isProtected = true)
      _ <- Game.eliminatePlayer(players(2), isEliminated = true)
      _ <- Game.startMatch()(basicRandomizer)
    } yield ()

    val newGame = modifyPlayersThenRestartMatch.exec(game)
    newGame.players.foreach { p =>
      p.isProtected should be(false)
      p.isEliminated should be(false)
    }
  }

  it should "have all players without a jester target, even if they had one in the previous match" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game(players)

    def modifyPlayersThenRestartMatch = for {
      _ <- Game.startMatch()(basicRandomizer)
      _ <- Game.addJesterTarget(playerName = players.head, targetName = players(1))
      _ <- Game.addJesterTarget(playerName = players(1), targetName = players(2))
      _ <- Game.addJesterTarget(playerName = players(2), targetName = players(1))
      _ <- Game.startMatch()(basicRandomizer)
    } yield ()

    val newGame = modifyPlayersThenRestartMatch.exec(game)
    newGame.players.foreach { p =>
      p.jesterTarget should be(None)
    }
  }

  it should "have no winner if more than one player is left and the discard is not empty" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game(players)

    def repeat[A, B](n: Int, s: State[A, B]): State[A, B] = if (n <= 1) s else s.flatMap(_ => repeat(n - 1, s))

    def playSome = for {
      _ <- Game.startMatch(Some(players.head))(basicRandomizer)
      _ <- repeat(3, Game.drawCard(players.head))
      _ <- Game.eliminatePlayer(players(2), isEliminated = true)
      winner <- Game.checkMatchOver(basicRandomizer)
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
      _ <- Game.startMatch(Some(players.head))(basicRandomizer)
      _ <- Game.eliminatePlayer(players.head, isEliminated = true)
      _ <- Game.eliminatePlayer(players(1), isEliminated = true)
      _ <- Game.eliminatePlayer(players(2), isEliminated = true)
      winner <- Game.checkMatchOver(basicRandomizer)
    } yield winner

    val newGame = eliminateEveryoneElse.exec(game)
    newGame.deck should not be game.deck
    newGame.players.foreach(_.isEliminated should be(false))
  }

  it should "have the first player of the next match be the winner of the last match" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game(players)

    def eliminateEveryoneElseThenStartNewMatch = for {
      _ <- Game.startMatch(Some(players.head))(basicRandomizer)
      _ <- Game.eliminatePlayer(players.head, isEliminated = true)
      _ <- Game.eliminatePlayer(players(1), isEliminated = true)
      _ <- Game.eliminatePlayer(players(2), isEliminated = true)
      winner <- Game.checkMatchOver(basicRandomizer)
      firstPlayer <- Game.currentPlayer
    } yield (winner, firstPlayer)

    val (winner, firstPlayer) = eliminateEveryoneElseThenStartNewMatch.eval(game)

    firstPlayer.name should be(winner.head.name)
  }

  it should "have the last remaining player be detected as the winner" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game(players)

    def eliminateEveryoneElse = for {
      _ <- Game.startMatch(Some(players.head))(basicRandomizer)
      _ <- Game.eliminatePlayer(players.head, isEliminated = true)
      _ <- Game.eliminatePlayer(players(1), isEliminated = true)
      _ <- Game.eliminatePlayer(players(2), isEliminated = true)
      winner <- Game.checkMatchOver(basicRandomizer)
    } yield winner

    val winner = eliminateEveryoneElse.eval(game)
    winner.head.name should be(players(3))
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

    // stack the deck for player 0 to get princess, and 2 to get countess
    val stackedDeck = Seq(Guard, Princess, Priest, Countess, Guard, Guard, Guard, Guard, Guard, Guard, Guard, Guard, Guard, Guard)
    val stackedRandomizer = Randomizer(
      shuffleDeck = (_) => stackedDeck,
      choosePlayer = (s: Seq[Player]) => s.head
    )

    val playTheGame = for {
      _ <- Game.startMatch(Some(players.head))(stackedRandomizer) //player(0) should get the 8, and player(2) should get the 7
      _ <- repeat(stackedDeck.length - 6, singleTurn)
      _ <- Game.eliminatePlayer(players.head, isEliminated = true)
      winner <- Game.checkMatchOver(basicRandomizer)
    } yield winner

    val (newGame, winner) = playTheGame(game)

    winner should not be empty
    winner.head.name should be(players(2))
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

    // stack the deck for player 0
    val stackedDeck = Seq(Guard, Princess, Guard, Guard, Guard, Guard, Guard, Guard, Guard, Guard, Guard, Guard)
    val stackedRandomizer = Randomizer(
      shuffleDeck = (_) => stackedDeck,
      choosePlayer = (s: Seq[Player]) => s.head
    )

    val playTheGame = for {
      _ <- Game.startMatch(Some(players.head))(stackedRandomizer)
      _ <- repeat(stackedDeck.length - 6, singleTurn)
      winner <- Game.checkMatchOver(basicRandomizer)
    } yield winner

    val (newGame, winner) = playTheGame(game)

    winner should not be empty
    winner.head.name should be(players.head)
    Game.getPlayer(players.head).eval(newGame).map(_.score).getOrElse(0) should be(1)
  }

  behavior of "The game deck"

  it should "be put in the order given by the shuffle function when shuffled" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game(players)

    val simpleShuffler = Randomizer(
      shuffleDeck = (s: Seq[Card]) => s.drop(4).reverse ++ s.take(4),
      choosePlayer = (s: Seq[Player]) => s.head
    )

    val (newGame, shuffledDeck) = Game.shuffle(simpleShuffler)(game)
    newGame.deck should be(shuffledDeck)
    shuffledDeck should not be Deck.cards
    shuffledDeck should contain theSameElementsAs Deck.cards
    shuffledDeck should be(simpleShuffler.shuffleDeck(Deck.cards))
  }

  it should "lose its top card when burnCard is called" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game(players)

    def shuffleAndBurn(r: Randomizer) = for {
      deck <- Game.shuffle(r)
      burntCard <- Game.burnCard
    } yield (deck, burntCard)

    val (newGame, (newDeck, burntCard)) = shuffleAndBurn(basicRandomizer)(game)
    burntCard should be(game.deck.head)
    newGame.deck should be(newDeck.tail)
    newGame.discard should be(game.discard)
    newGame.visibleDiscard should be(game.visibleDiscard)
  }

  it should "return the burned card when burncard is called" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game(players)

    def shuffleAndBurn(r: Randomizer) = for {
      deck <- Game.shuffle(r)
      burntCard <- Game.burnCard
    } yield (deck, burntCard)

    val (newGame, (newDeck, burntCard)) = shuffleAndBurn(basicRandomizer)(game)
    Seq(burntCard) ++ newGame.deck should be(newDeck)
  }

  it should "not have any change in the discard piles after burning" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game(players)

    def shuffleAndBurn(r: Randomizer) = for {
      deck <- Game.shuffle(r)
      burntCard <- Game.burnCard
    } yield (deck, burntCard)

    val newGame = shuffleAndBurn(basicRandomizer).exec(game)
    newGame.discard should be(game.discard)
    newGame.visibleDiscard should be(game.visibleDiscard)
  }

  it should "show the burned card if burnCardVisible is called" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game(players)

    def shuffleAndBurn(r: Randomizer) = for {
      _ <- Game.shuffle(r)
      burntCard <- Game.burnCardVisible
    } yield burntCard

    val (newGame, burntCard) = shuffleAndBurn(basicRandomizer)(game)
    newGame.discard should be(game.discard)
    newGame.visibleDiscard should contain theSameElementsAs burntCard
  }

  it should "no longer have its top card when a player draws" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game(players)

    def shuffleAndDraw(r: Randomizer) = for {
      startingDeck <- Game.shuffle(r)
      _ <- Game.drawCard(players.head)
    } yield startingDeck

    val (newGame, startingDeck) = shuffleAndDraw(basicRandomizer)(game)
    newGame.deck should be(startingDeck.tail)
  }

  behavior of "A player"

  it should "gain the top card of the deck when they draw" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game(players)

    def shuffleAndDraw(r: Randomizer) = for {
      startingDeck <- Game.shuffle(r)
      newPlayer <- Game.drawCard(players.head)
    } yield (startingDeck, newPlayer)

    val (newGame, (startingDeck, newPlayer)) = shuffleAndDraw(basicRandomizer)(game)
    newGame.players.head should be(newPlayer.get)
    newPlayer.get.hand should contain only startingDeck.head
  }

  it should "not gain cards when another player draws" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game(players)

    def shuffleAndDraw(r: Randomizer) = for {
      _ <- Game.shuffle(r)
      _ <- Game.drawCard(players.head)
    } yield ()

    val newGame = shuffleAndDraw(basicRandomizer).exec(game)
    newGame.players.tail.foreach(p => {
      p.hand shouldBe empty
    })
  }

  it should "no longer have a card in their hand after discarding" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game(players)

    def shuffleDrawAndDiscard(r: Randomizer) = for {
      _ <- Game.shuffle(r)
      player <- Game.drawCard(players.head)
      _ <- Game.playerDiscard(players.head, player.get.hand.head)
    } yield player

    val (newGame, player) = shuffleDrawAndDiscard(basicRandomizer)(game)
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
      _ <- Game.awardPoint(players.head)
      _ <- Game.awardPoint(players(1))
      _ <- Game.awardPoint(players(2))
      _ <- Game.awardPoint(players(2))
      ty <- Game.getPlayer(players.head)
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
      ty <- Game.eliminatePlayer(players.head, isEliminated = true)
      kev <- Game.eliminatePlayer(players(1), isEliminated = true)
    } yield (ty.get, kev.get)

    val (newGame, (ty, kev)) = eliminateSome(game)
    ty.isEliminated should be(true)
    kev.isEliminated should be(true)
    Game.getPlayer(players.head).eval(newGame).get.isEliminated should be(true)
    Game.getPlayer(players(1)).eval(newGame).get.isEliminated should be(true)
    Game.getPlayer(players(2)).eval(newGame).get.isEliminated should be(false)
    Game.getPlayer(players(3)).eval(newGame).get.isEliminated should be(false)
  }

  it should "have their card discarded if they are eliminated" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game.startMatch(Some(players.head))(basicRandomizer).exec(Game(players))

    def checkKevinsCardThenEliminate = for {
      kev <- Game.getPlayer(players(1))
      _ <- Game.eliminatePlayer(players(1), isEliminated = true)
    } yield kev.get.hand.head

    val (newGame, kevsCard) = checkKevinsCardThenEliminate(game)

    newGame.discard.head should be(kevsCard)
  }

  it should "not change the state of the game if a user not in the game is eliminated" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game(players)

    Game.eliminatePlayer("BADPLAYER", isEliminated = true).exec(game) should be(game)
  }

  it should "be protected if protect is called on them" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game(players)

    def protectSome = for {
      ty <- Game.protectPlayer(players.head, isProtected = true)
      kev <- Game.protectPlayer(players(1), isProtected = true)
    } yield (ty.get, kev.get)

    val (newGame, (ty, kev)) = protectSome(game)
    ty.isProtected should be(true)
    kev.isProtected should be(true)
    Game.getPlayer(players.head).eval(newGame).get.isProtected should be(true)
    Game.getPlayer(players(1)).eval(newGame).get.isProtected should be(true)
    Game.getPlayer(players(2)).eval(newGame).get.isProtected should be(false)
    Game.getPlayer(players(3)).eval(newGame).get.isProtected should be(false)
  }

  it should "no longer be protected one their turn starts" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // use this randomizer for tests that need to ignore card effects. This will give the first player a countess
    val noEffectRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, Countess) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    val game = Game.startMatch(Some(players.head))(noEffectRandomizer).exec(Game(players))

    def protectPlayerThenStartTheirTurn = for {
      p1 <- Game.currentPlayer
      p2 <- Game.protectPlayer(players(1), isProtected = true)
      _ <- Game.processTurn(p1.name, p1.hand.head)(basicRandomizer)
      p3 <- Game.getPlayer(players(1))
    } yield (p2.get, p3.get)

    val (protectedPlayer, unprotectedPlayer) = protectPlayerThenStartTheirTurn.eval(game)

    protectedPlayer.isProtected should be(true)
    unprotectedPlayer.isProtected should be(false)
  }

  it should "give a player a jester target when called" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game(players)

    def protectSome = for {
      ty <- Game.addJesterTarget(players.head, players(1))
      kev <- Game.addJesterTarget(players(1), players.head)
    } yield (ty.get, kev.get)

    val (newGame, (ty, kev)) = protectSome(game)
    ty.jesterTarget should be(Some(players(1)))
    kev.jesterTarget should be(Some(players.head))
    Game.getPlayer(players.head).eval(newGame).get.jesterTarget should be(Some(players(1)))
    Game.getPlayer(players(1)).eval(newGame).get.jesterTarget should be(Some(players.head))
    Game.getPlayer(players(2)).eval(newGame).get.jesterTarget should be(None)
    Game.getPlayer(players(3)).eval(newGame).get.jesterTarget should be(None)
  }

  it should "return None and not change game state if a player not in the game gets a jester target" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game(players)

    val (newGame, p) = Game.addJesterTarget("BADPLAYER", "Kevin")(game)
    p should be(None)
    newGame should be(game)
  }

  it should "return None and not change game state if a player not in the game is targeted by jester" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game(players)

    val (newGame, p) = Game.addJesterTarget("Tyler", "BADPLAYER")(game)
    p should be(None)
    newGame should be(game)
  }

  it should "be skipped in the turn order if they are eliminated" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // use this randomizer for tests that need to ignore card effects. This will give the first player a countess
    val noEffectRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, Countess) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    val game = Game.startMatch(Some(players.head))(noEffectRandomizer).exec(Game(players))

    def eliminateThenSwitchTurns = for {
      _ <- Game.eliminatePlayer(players(1), isEliminated = true)
      p1 <- Game.currentPlayer
      _ <- Game.processTurn(p1.name, p1.hand.head)(noEffectRandomizer)
      p2 <- Game.currentPlayer
    } yield p2

    val nextPlayer = eliminateThenSwitchTurns.eval(game)
    nextPlayer.name should be(players(2))
  }

  it should "not change the state of the game if a user not in the game is protected" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game(players)

    Game.protectPlayer("BADPLAYER", isProtected = true).exec(game) should be(game)
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

    def shuffleDrawAndDiscard(r: Randomizer) = for {
      _ <- Game.shuffle(r)
      player <- Game.drawCard(players.head)
      discard <- Game.playerDiscard(players.head, player.get.hand.head)
    } yield (player, discard)

    val (newGame, (player, discard)) = shuffleDrawAndDiscard(basicRandomizer)(game)
    newGame.discard should be(discard)
    discard.head should be(player.get.hand.head)
  }

  it should "remain the same if a player that doesn't exist tries to discard a card" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game(players)

    def drawAndDiscardInvalidCard = for {
      player <- Game.drawCard(players.head)
      discard <- Game.playerDiscard("BADPLAYER", Princess)
    } yield (player, discard)

    val (player, discard) = drawAndDiscardInvalidCard.eval(game)
    player.get.hand.head should be(Guard)
    discard shouldBe empty

  }

  it should "remain the same if a player tries to discard a card they don't have" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game(players)

    def drawAndDiscardInvalidCard = for {
      player <- Game.drawCard(players.head) //should draw a guard
      discard <- Game.playerDiscard(players.head, Princess)
    } yield (player, discard)

    val (player, discard) = drawAndDiscardInvalidCard.eval(game)
    player.get.hand.head should be(Guard)
    discard shouldBe empty

  }

  behavior of "taking a turn"

  it should "not change the game if a player not in the game tries to go" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    val game = Game.startMatch(Some(players.head))(basicRandomizer).exec(Game(players))

    val newGame = Game.processTurn("BADPLAYER", Countess)(basicRandomizer).exec(game)

    newGame should be(game)
  }

  it should "not change the game if it's not the player's turn" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // use this randomizer for tests that need to ignore card effects. This will give everyone a countess
    val noEffectRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, Countess, Countess, Countess, Countess, Countess, Countess, Countess) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    //there will be no side effects from the discard
    val game = Game.startMatch(Some(players.head))(noEffectRandomizer).exec(Game(players))

    val newGame = (for {
      p <- Game.getPlayer(players(3))
      _ <- Game.processTurn(players(3), p.get.hand.head)(noEffectRandomizer)
    } yield ()).exec(game)

    newGame should be(game)
  }

  it should "not change the game if the player doesn't have the cards he wants to discard" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // this randomizer will give the first player a princess
    val princessRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, Princess) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    val game = Game.startMatch(Some(players.head))(princessRandomizer).exec(Game(players))

    val newGame = (for {
      p <- Game.currentPlayer
      _ <- Game.processTurn(p.name, Countess)(princessRandomizer) //not the card in hand
    } yield ()).exec(game)

    newGame should be(game)
  }

  it should "be the next player's turn after the current player takes a turn" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // use this randomizer for tests that need to ignore card effects. This will give everyone a countess
    val noEffectRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, Countess, Countess, Countess, Countess, Countess, Countess, Countess) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    val game = Game.startMatch(Some(players.head))(noEffectRandomizer).exec(Game(players))

    def takeATurnThenGetNextPlayer = for {
      p <- Game.currentPlayer
      _ <- Game.processTurn(p.name, p.hand.head)(noEffectRandomizer)
      p2 <- Game.currentPlayer
    } yield p2

    val (_, nextPlayer) = takeATurnThenGetNextPlayer(game)

    nextPlayer.name should be(players(1))
  }

  it should "cause the next player to draw a card after the current player takes a turn" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // use this randomizer for tests that need to ignore card effects. This will give everyone a countess
    val noEffectRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, Countess, Countess, Countess, Countess, Countess, Countess, Countess) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    val game = Game.startMatch(Some(players.head))(noEffectRandomizer).exec(Game(players))

    def takeATurnThenGetNextPlayer = for {
      p <- Game.currentPlayer
      _ <- Game.processTurn(p.name, p.hand.head)(noEffectRandomizer)
      p2 <- Game.currentPlayer
    } yield p2

    val (_, nextPlayer) = takeATurnThenGetNextPlayer(game)

    nextPlayer.name should be(players(1))
    nextPlayer.hand should have length 2
  }

  it should "add the discarded card to the discard pile" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // use this randomizer for tests that need to ignore card effects. This will give everyone a countess
    val noEffectRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, Countess, Countess, Countess, Countess, Countess, Countess, Countess) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    val game = Game.startMatch(Some(players.head))(noEffectRandomizer).exec(Game(players))

    def takeATurn = for {
      p <- Game.currentPlayer
      _ <- Game.processTurn(p.name, p.hand.head)(noEffectRandomizer)
    } yield p.hand.head

    val (newGame, discard) = takeATurn(game)

    newGame.discard.head should be(discard)
  }

  it should "detect and return the winner of a match" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // use this randomizer for tests that need to ignore card effects. This will give everyone a countess
    val noEffectRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, Countess, Countess, Countess, Countess, Countess, Countess, Countess) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    val game = Game.startMatch(Some(players.head))(noEffectRandomizer).exec(Game(players))

    def makeSomeoneWinnerThenTakeTurn = for {
      _ <- Game.eliminatePlayer(players(1), isEliminated = true)
      _ <- Game.eliminatePlayer(players(2), isEliminated = true)
      _ <- Game.eliminatePlayer(players(3), isEliminated = true)
      p <- Game.currentPlayer
      winners <- Game.processTurn(p.name, p.hand.head)(noEffectRandomizer)
    } yield winners

    val winners = makeSomeoneWinnerThenTakeTurn.eval(game)

    winners should matchPattern { case MatchOver(_, "Tyler", Nil, _) => }
  }

  it should "start a new match if there is a winner" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // use this randomizer for tests that need to ignore card effects. This will give everyone a countess
    val noEffectRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, Countess, Countess, Countess, Countess, Countess, Countess, Countess) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )

    val predictableRandomizer = Randomizer(
      shuffleDeck = (_) => Seq(Guard, Prince, Guard, Handmaid, Priest, Prince, Priest, Guard, Guard, Baron, Baron, Guard, Princess, Countess, King, Handmaid),
      choosePlayer = (s: Seq[Player]) => s.head
    )
    val game = Game.startMatch(Some(players.head))(noEffectRandomizer).exec(Game(players))

    def makeSomeoneWinner = for {
      _ <- Game.eliminatePlayer(players(1), isEliminated = true)
      _ <- Game.eliminatePlayer(players(2), isEliminated = true)
      _ <- Game.eliminatePlayer(players(3), isEliminated = true)
    } yield ()

    val newGame1 = makeSomeoneWinner.exec(game)

    def takeTurn = for {
      p <- Game.currentPlayer
      _ <- Game.processTurn(p.name, p.hand.head)(predictableRandomizer)
    } yield ()

    val newGame2 = takeTurn.exec(newGame1)

    newGame1.deck should not be newGame2.deck
    newGame2.players.foreach(_.isEliminated should be(false))
    //To prove that a new match has started, shuffle a new deck with the same randomizer as the new match.
    //If the decks have the same tails, that shows that there's a new game
    Game.shuffle(predictableRandomizer).exec(Game(players)).deck.endsWith(newGame2.deck) should be(true)

  }

  it should "detect and return the winner of a game" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // use this randomizer for tests that need to ignore card effects. This will give everyone a countess
    val noEffectRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, Countess, Countess, Countess, Countess, Countess, Countess, Countess) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    //Use 24 as a seed for tests that need to ignore card effects. This will give the first player a countess
    val game = Game.startMatch(Some(players.head))(noEffectRandomizer).exec(Game(players))

    def makeSomeoneWinnerThenTakeTurn = for {
      _ <- Game.eliminatePlayer(players(1), isEliminated = true)
      _ <- Game.eliminatePlayer(players(2), isEliminated = true)
      _ <- Game.eliminatePlayer(players(3), isEliminated = true)
      _ <- Game.awardPoint(players.head)
      _ <- Game.awardPoint(players.head)
      _ <- Game.awardPoint(players.head)
      p <- Game.currentPlayer
      winners <- Game.processTurn(p.name, p.hand.head)(noEffectRandomizer)
    } yield winners

    val winners = makeSomeoneWinnerThenTakeTurn.eval(game)

    winners should matchPattern { case GameOver(_, "Tyler", Nil, "Tyler") => }
  }

  it should "award multiple points at the end of a match if necessary" in (pending)

  it should "detect if multiple players have won a game" in (pending)

  //TODO: test that processturn will detect a match winner and a game winner

  behavior of "The guard card"

  it should "eliminate a player if their card is guessed" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // use this randomizer for tests that need specific card
    val stackedDeckRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, Guard, Priest) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    //player 1 will get a guard, player 2 a priest
    val game = Game.startMatch(Some(players.head))(stackedDeckRandomizer).exec(Game(players))

    def playGuardAndGuessRight = for {
      p <- Game.currentPlayer
      result <- Game.processTurn(p.name, Guard, Some(players(1)), Some(Priest))(stackedDeckRandomizer)
      target <- Game.getPlayer(players(1))
    } yield (result, target.get)

    val (newGame, (message, target)) = playGuardAndGuessRight(game)

    message should matchPattern { case NextTurn(_, _) => }
    target.isEliminated should be(true)
    newGame should not be game
  }

  it should "not eliminate a player if their card is guessed incorrectly" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // use this randomizer for tests that need specific card
    val stackedDeckRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, Guard, Priest) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    //player 1 will get a guard, player 2 a priest
    val game = Game.startMatch(Some(players.head))(stackedDeckRandomizer).exec(Game(players))

    def playGuardAndGuessWrong = for {
      p <- Game.currentPlayer
      result <- Game.processTurn(p.name, Guard, Some(players(1)), Some(Baron))(stackedDeckRandomizer) //WRONG
      target <- Game.getPlayer(players(1))
    } yield (result, target.get)

    val (newGame, (message, target)) = playGuardAndGuessWrong(game)

    message should matchPattern { case NextTurn(_, _) => }
    target.isEliminated should be(false)
    newGame should not be game
  }

  it should "fail if guard is guessed" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // use this randomizer for tests that need specific card
    val stackedDeckRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, Guard, Guard) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    //player 1 will get a guard, player 1 also a guard
    val game = Game.startMatch(Some(players.head))(stackedDeckRandomizer).exec(Game(players))

    def playGuardAndGuessGuard = for {
      p <- Game.currentPlayer
      result <- Game.processTurn(p.name, Guard, Some(players(1)), Some(Guard))(stackedDeckRandomizer)
      currentPlayer <- Game.currentPlayer
    } yield (result, currentPlayer)

    val (newGame, (message, currentPlayer)) = playGuardAndGuessGuard(game)

    message should matchPattern { case PlayError(_) => }
    currentPlayer.name should be(players.head)
    newGame should be(game)
  }

  it should "fail if a player not in the game is targeted" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // use this randomizer for tests that need specific card
    val stackedDeckRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, Guard) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    //player 1 will get a guard
    val game = Game.startMatch(Some(players.head))(stackedDeckRandomizer).exec(Game(players))

    def playGuardAndGuessBadplayer = for {
      p <- Game.currentPlayer
      result <- Game.processTurn(p.name, Guard, Some("BADPLAYER"), Some(Priest))(stackedDeckRandomizer)
      currentPlayer <- Game.currentPlayer
    } yield (result, currentPlayer)

    val (newGame, (message, currentPlayer)) = playGuardAndGuessBadplayer(game)

    message should matchPattern { case PlayError(_) => }
    currentPlayer.name should be(players.head)
    newGame should be(game)
  }

  it should "fail if the targeted player is protected" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // use this randomizer for tests that need specific card
    val stackedDeckRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, Guard, Priest) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    //player 1 will get a guard, player 2 a priest
    val game = Game.startMatch(Some(players.head))(stackedDeckRandomizer).exec(Game(players))

    def protectThenPlayGuard = for {
      _ <- Game.protectPlayer(players(1), isProtected = true)
      p <- Game.currentPlayer
      result <- Game.processTurn(p.name, Guard, Some(players(1)), Some(Priest))(stackedDeckRandomizer)
      currentPlayer <- Game.currentPlayer
    } yield (result, currentPlayer)

    val (message, currentPlayer) = protectThenPlayGuard.eval(game)

    message should matchPattern { case PlayError(_) => }
    currentPlayer.name should be(players.head)
  }

  it should "fail if the targeted player is eliminated" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // use this randomizer for tests that need specific card
    val stackedDeckRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, Guard, Priest) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    //player 1 will get a guard, player 2 a priest
    val game = Game.startMatch(Some(players.head))(stackedDeckRandomizer).exec(Game(players))

    def protectThenPlayGuard = for {
      _ <- Game.eliminatePlayer(players(1), isEliminated = true)
      p <- Game.currentPlayer
      result <- Game.processTurn(p.name, Guard, Some(players(1)), Some(Priest))(stackedDeckRandomizer)
      currentPlayer <- Game.currentPlayer
    } yield (result, currentPlayer)

    val (message, currentPlayer) = protectThenPlayGuard.eval(game)

    message should matchPattern { case PlayError(_) => }
    currentPlayer.name should be(players.head)
  }

  it should "allow the player to play with no effect if every other player is eliminated or protected" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // use this randomizer for tests that need specific card
    val stackedDeckRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, Guard, Priest) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    //player 1 will get a guard, player 2 a priest
    val game = Game.startMatch(Some(players.head))(stackedDeckRandomizer).exec(Game(players))

    def protectOrEliminateEveryoneThenPlay = for {
      _ <- Game.protectPlayer(players(1), isProtected = true)
      _ <- Game.eliminatePlayer(players(2), isEliminated = true)
      _ <- Game.protectPlayer(players(3), isProtected = true)
      result <- Game.processTurn(players.head, Guard, None, None)(stackedDeckRandomizer)
      p <- Game.currentPlayer
    } yield (p, result)

    val (nextPlayer, result) = protectOrEliminateEveryoneThenPlay.eval(game)
    inside(result) { case NextTurn(_, p) =>
      p should be("Kevin")
    }
    nextPlayer.name should be(players(1))

  }

  it should "fail if a target or a guess is not supplied" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // use this randomizer for tests that need specific card
    val stackedDeckRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, Guard, Priest) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    //player 1 will get a guard, player 2 a priest
    val game = Game(players)
    val (game1, result1) = Game.processTurn(players.head, Guard, None, Some(Priest))(stackedDeckRandomizer).apply(game)
    game1 should be(game)
    result1 should matchPattern { case PlayError(_) => }

    val (game2, result2) = Game.processTurn(players.head, Guard, Some(players(2)), None)(stackedDeckRandomizer).apply(game)
    game2 should be(game)
    result2 should matchPattern { case PlayError(_) => }
  }

  it should "fail if the player targets themself" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // use this randomizer for tests that need specific card
    val stackedDeckRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, Guard, Priest, Handmaid, Prince, Princess) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    //player 1 will get a guard and a Princess, player 2 a priest
    val game = Game.startMatch(Some(players.head))(stackedDeckRandomizer).exec(Game(players))
    val (newGame, result1) = Game.processTurn(players.head, Guard, Some(players.head), Some(Princess))(stackedDeckRandomizer).apply(game)
    newGame should be(game)
    result1 should matchPattern { case PlayError(_) => }
  }

  behavior of "The priest card"

  it should "show the card of the targeted player in the returned message" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // use this randomizer for tests that need specific card
    val stackedDeckRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, Priest, Guard) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    //player 1 will get a priest, player 2 a guard
    val game = Game.startMatch(Some(players.head))(stackedDeckRandomizer).exec(Game(players))

    def peekAtPlayersCard = for {
      p <- Game.currentPlayer
      result <- Game.processTurn(p.name, Priest, Some(players(1)), None)(stackedDeckRandomizer)
    } yield result

    val message = peekAtPlayersCard.eval(game)

    message should matchPattern { case NextTurn(_, _) => }
    message.asInstanceOf[NextTurn].lastTurnResult.msg should include(Guard.name)
  }

  it should "fail if a player not in the game is targeted" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // use this randomizer for tests that need specific card
    val stackedDeckRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, Priest, Guard) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    //player 1 will get a priest, player 2 a guard
    val game = Game.startMatch(Some(players.head))(stackedDeckRandomizer).exec(Game(players))

    def playPriestOnBadplayer = for {
      p <- Game.currentPlayer
      result <- Game.processTurn(p.name, Priest, Some("BADPLAYER"), None)(stackedDeckRandomizer)
      currentPlayer <- Game.currentPlayer
    } yield (result, currentPlayer)

    val (newGame, (message, currentPlayer)) = playPriestOnBadplayer(game)

    message should matchPattern { case PlayError(_) => }
    currentPlayer.name should be(players.head)
    newGame should be(game)
  }

  it should "fail if the targeted player is protected" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // use this randomizer for tests that need specific card
    val stackedDeckRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, Priest, Guard) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    //player 1 will get a priest, player 2 a guard
    val game = Game.startMatch(Some(players.head))(stackedDeckRandomizer).exec(Game(players))

    def playPriestOnProtectedPlayer = for {
      _ <- Game.protectPlayer(players(1), isProtected = true)
      p <- Game.currentPlayer
      result <- Game.processTurn(p.name, Priest, Some(players(1)), None)(stackedDeckRandomizer)
      currentPlayer <- Game.currentPlayer
    } yield (result, currentPlayer)

    val (message, currentPlayer) = playPriestOnProtectedPlayer.eval(game)

    message should matchPattern { case PlayError(_) => }
    currentPlayer.name should be(players.head)
  }

  it should "fail if the targeted player is eliminated" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // use this randomizer for tests that need specific card
    val stackedDeckRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, Priest, Guard) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    //player 1 will get a priest, player 2 a guard
    val game = Game.startMatch(Some(players.head))(stackedDeckRandomizer).exec(Game(players))

    def playPriestOnEliminatedPlayer = for {
      _ <- Game.eliminatePlayer(players(1), isEliminated = true)
      p <- Game.currentPlayer
      result <- Game.processTurn(p.name, Priest, Some(players(1)), None)(stackedDeckRandomizer)
      currentPlayer <- Game.currentPlayer
    } yield (result, currentPlayer)

    val (message, currentPlayer) = playPriestOnEliminatedPlayer.eval(game)

    message should matchPattern { case PlayError(_) => }
    currentPlayer.name should be(players.head)
  }

  it should "allow the player to play with no effect if every other player is eliminated or protected" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // use this randomizer for tests that need specific card
    val stackedDeckRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, Priest, Guard) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    //player 1 will get a priest, player 2 a guard
    val game = Game.startMatch(Some(players.head))(stackedDeckRandomizer).exec(Game(players))

    def protectOrEliminateEveryoneThenPlay = for {
      _ <- Game.protectPlayer(players(1), isProtected = true)
      _ <- Game.eliminatePlayer(players(2), isEliminated = true)
      _ <- Game.protectPlayer(players(3), isProtected = true)
      result <- Game.processTurn(players.head, Priest, None, None)(stackedDeckRandomizer)
      p <- Game.currentPlayer
    } yield (p, result)

    val (nextPlayer, result) = protectOrEliminateEveryoneThenPlay.eval(game)
    inside(result) { case NextTurn(_, p) =>
      p should be("Kevin")
    }
    nextPlayer.name should be(players(1))

  }

  it should "fail if a target is not supplied" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // use this randomizer for tests that need specific card
    val stackedDeckRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, Priest) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    //player 1 will get a priest
    val game = Game(players)
    val (game1, result1) = Game.processTurn(players.head, Priest, None, None)(stackedDeckRandomizer).apply(game)
    game1 should be(game)
    result1 should matchPattern { case PlayError(_) => }
  }

  it should "fail if the player targets themself" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // use this randomizer for tests that need specific card
    val stackedDeckRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, Priest) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    //player 1 will get a priest
    val game = Game.startMatch(Some(players.head))(stackedDeckRandomizer).exec(Game(players))
    val (newGame, result1) = Game.processTurn(players.head, Priest, Some(players.head), None)(stackedDeckRandomizer).apply(game)
    newGame should be(game)
    result1 should matchPattern { case PlayError(_) => }
  }

  behavior of "The baron card"

  it should "eliminate the target player if your card is higher than theirs" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // use this randomizer for tests that need specific card
    val stackedDeckRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, Baron, Guard, Princess, Handmaid, Countess) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    //player 1 will get a baron and a countess, player 2 a guard
    val game = Game.startMatch(Some(players.head))(stackedDeckRandomizer).exec(Game(players))

    val (newGame, result) = Game.processTurn(players.head, Baron, Some(players(1)), None)(stackedDeckRandomizer).apply(game)

    result should matchPattern { case NextTurn(_, _) => }
    Game.getPlayer(players.head).eval(newGame).get.isEliminated should be(false)
    Game.getPlayer(players(1)).eval(newGame).get.isEliminated should be(true)
    Game.currentPlayer.eval(newGame).name should be(players(2))

  }

  it should "eliminate you if the target players card is higher than yours" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // use this randomizer for tests that need specific card
    val stackedDeckRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, Baron, Prince, Princess, Handmaid, Priest) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    //player 1 will get a baron and a priest, player 2 a Prince
    val game = Game.startMatch(Some(players.head))(stackedDeckRandomizer).exec(Game(players))

    val (newGame, result) = Game.processTurn(players.head, Baron, Some(players(1)), None)(stackedDeckRandomizer).apply(game)

    result should matchPattern { case NextTurn(_, _) => }
    Game.getPlayer(players.head).eval(newGame).get.isEliminated should be(true)
    Game.getPlayer(players(1)).eval(newGame).get.isEliminated should be(false)
    Game.currentPlayer.eval(newGame).name should be(players(1))

  }

  it should "eliminate nobody if you have the same card" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // use this randomizer for tests that need specific card
    val stackedDeckRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, Baron, Handmaid, Princess, Handmaid, Handmaid) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    //player 1 will get a baron and a handmaid, player 2 a handmaid
    val game = Game.startMatch(Some(players.head))(stackedDeckRandomizer).exec(Game(players))

    val (newGame, result) = Game.processTurn(players.head, Baron, Some(players(1)), None)(stackedDeckRandomizer).apply(game)

    result should matchPattern { case NextTurn(_, _) => }
    Game.getPlayer(players.head).eval(newGame).get.isEliminated should be(false)
    Game.getPlayer(players(1)).eval(newGame).get.isEliminated should be(false)
    Game.currentPlayer.eval(newGame).name should be(players(1))

  }

  it should "fail if a player not in the game is targeted" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // use this randomizer for tests that need specific card
    val stackedDeckRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, Baron, Guard, Princess, Handmaid, Prince) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    //player 1 will get a baron and a prince, player 2 a guard
    val game = Game.startMatch(Some(players.head))(stackedDeckRandomizer).exec(Game(players))

    def playBaronOnBadPlayer = for {
      p <- Game.currentPlayer
      result <- Game.processTurn(p.name, Baron, Some("BADPLAYER"), None)(stackedDeckRandomizer)
      currentPlayer <- Game.currentPlayer
    } yield (result, currentPlayer)

    val (newGame, (message, currentPlayer)) = playBaronOnBadPlayer(game)

    message should matchPattern { case PlayError(_) => }
    currentPlayer.name should be(players.head)
    newGame should be(game)
  }

  it should "fail if the targeted player is protected" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // use this randomizer for tests that need specific card
    val stackedDeckRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, Baron, Guard, Princess, Handmaid, Prince) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    //player 1 will get a baron and a prince, player 2 a guard
    val game = Game.startMatch(Some(players.head))(stackedDeckRandomizer).exec(Game(players))

    def playBaronOnProtectedPlayer = for {
      _ <- Game.protectPlayer(players(1), isProtected = true)
      p <- Game.currentPlayer
      result <- Game.processTurn(p.name, Baron, Some(players(1)), None)(stackedDeckRandomizer)
      currentPlayer <- Game.currentPlayer
    } yield (result, currentPlayer)

    val (message, currentPlayer) = playBaronOnProtectedPlayer.eval(game)

    message should matchPattern { case PlayError(_) => }
    currentPlayer.name should be(players.head)
  }

  it should "fail if the targeted player is eliminated" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // use this randomizer for tests that need specific card
    val stackedDeckRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, Baron, Guard, Princess, Handmaid, Prince) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    //player 1 will get a baron and a prince, player 2 a guard
    val game = Game.startMatch(Some(players.head))(stackedDeckRandomizer).exec(Game(players))

    def playBaronOnEliminatedPlayer = for {
      _ <- Game.eliminatePlayer(players(1), isEliminated = true)
      p <- Game.currentPlayer
      result <- Game.processTurn(p.name, Baron, Some(players(1)), None)(stackedDeckRandomizer)
      currentPlayer <- Game.currentPlayer
    } yield (result, currentPlayer)

    val (message, currentPlayer) = playBaronOnEliminatedPlayer.eval(game)

    message should matchPattern { case PlayError(_) => }
    currentPlayer.name should be(players.head)
  }

  it should "allow the player to play with no effect if every other player is eliminated or protected" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // use this randomizer for tests that need specific card
    val stackedDeckRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, Baron, Guard, Princess, Handmaid, Prince) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    //player 1 will get a baron and a prince, player 2 a guard
    val game = Game.startMatch(Some(players.head))(stackedDeckRandomizer).exec(Game(players))

    def protectOrEliminateEveryoneThenPlay = for {
      _ <- Game.protectPlayer(players(1), isProtected = true)
      _ <- Game.eliminatePlayer(players(2), isEliminated = true)
      _ <- Game.protectPlayer(players(3), isProtected = true)
      result <- Game.processTurn(players.head, Baron, None, None)(stackedDeckRandomizer)
      p <- Game.currentPlayer
    } yield (p, result)

    val (nextPlayer, result) = protectOrEliminateEveryoneThenPlay.eval(game)
    inside(result) { case NextTurn(_, p) =>
      p should be("Kevin")
    }
    nextPlayer.name should be(players(1))

  }

  it should "fail if a target is not supplied" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // use this randomizer for tests that need specific card
    val stackedDeckRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, Baron, Guard, Princess, Handmaid, Prince) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    //player 1 will get a baron and a prince, player 2 a guard
    val game = Game(players)
    val (game1, result1) = Game.processTurn(players.head, Baron, None, None)(stackedDeckRandomizer).apply(game)
    game1 should be(game)
    result1 should matchPattern { case PlayError(_) => }
  }

  it should "fail if the player targets themself" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // use this randomizer for tests that need specific card
    val stackedDeckRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, Baron) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    //player 1 will get a Baron
    val game = Game.startMatch(Some(players.head))(stackedDeckRandomizer).exec(Game(players))
    val (newGame, result1) = Game.processTurn(players.head, Baron, Some(players.head), None)(stackedDeckRandomizer).apply(game)
    newGame should be(game)
    result1 should matchPattern { case PlayError(_) => }
  }

  behavior of "The handmaid card"

  it should "cause the player to become protected" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // use this randomizer for tests that need specific card
    val stackedDeckRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, Handmaid) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    //player 1 will get a Handmaid
    val game = Game.startMatch(Some(players.head))(stackedDeckRandomizer).exec(Game(players))

    def playHandmaid = for {
      p <- Game.currentPlayer
      result <- Game.processTurn(p.name, Handmaid, None, None)(stackedDeckRandomizer)
      p1 <- Game.getPlayer(players.head)
    } yield (p1.get, result)

    val (protectedPlayer, message) = playHandmaid.eval(game)

    message should matchPattern { case NextTurn(_, _) => }
    protectedPlayer.isProtected should be(true)

  }

  behavior of "The prince card"

  it should "cause the targeted player to discard their hand" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // use this randomizer for tests that need specific card
    val stackedDeckRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, Prince, Guard) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    //player 1 will get a prince, player 2 a guard
    val game = Game.startMatch(Some(players.head))(stackedDeckRandomizer).exec(Game(players))

    def playPrinceOnPlayer = for {
      p <- Game.currentPlayer
      result <- Game.processTurn(p.name, Prince, Some(players(1)), None)(stackedDeckRandomizer)
      currentPlayer <- Game.currentPlayer
    } yield (result, currentPlayer)

    val (newGame, (message, currentPlayer)) = playPrinceOnPlayer(game)

    inside(message) { case NextTurn(_, p) =>
      p should be("Kevin")
    }
    currentPlayer.name should be(players(1))
    newGame.discard should contain(Prince)
    newGame.discard should contain(Guard)
  }

  it should "cause the player to draw the burn card if they are targeted while there is nothing in the draw pile" in {
    val game = Game(players = Seq(Player(name = "Tyler", hand = Seq(Prince, Prince)), Player(name = "Kevin", hand = Seq(Guard))),
      deck = Nil,
      burnCard = Seq(Princess)
    )

    val results = Game.processTurn("Tyler", Prince, Some("Kevin"), None)(basicRandomizer).eval(game)

    //because the burn card was a Princess, players(1) should have drawn it and won the last round
    results should matchPattern { case MatchOver(_, "Kevin", Nil, _) => }
  }

  it should "fail if the targeted player is protected" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // use this randomizer for tests that need specific card
    val stackedDeckRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, Prince, Guard) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    //player 1 will get a prince, player 2 a guard
    val game = Game.startMatch(Some(players.head))(stackedDeckRandomizer).exec(Game(players))

    def playPrinceOnProtectedPlayer = for {
      _ <- Game.protectPlayer(players(1), isProtected = true)
      p <- Game.currentPlayer
      result <- Game.processTurn(p.name, Prince, Some(players(1)), None)(stackedDeckRandomizer)
      currentPlayer <- Game.currentPlayer
    } yield (result, currentPlayer)

    val (message, currentPlayer) = playPrinceOnProtectedPlayer.eval(game)

    message should matchPattern { case PlayError(_) => }
    currentPlayer.name should be(players.head)
  }

  it should "fail if the targeted player is eliminated" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // use this randomizer for tests that need specific card
    val stackedDeckRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, Prince, Guard) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    //player 1 will get a prince, player 2 a guard
    val game = Game.startMatch(Some(players.head))(stackedDeckRandomizer).exec(Game(players))

    def playPrinceOnEliminatedPlayer = for {
      _ <- Game.eliminatePlayer(players(1), isEliminated = true)
      p <- Game.currentPlayer
      result <- Game.processTurn(p.name, Prince, Some(players(1)), None)(stackedDeckRandomizer)
      currentPlayer <- Game.currentPlayer
    } yield (result, currentPlayer)

    val (message, currentPlayer) = playPrinceOnEliminatedPlayer.eval(game)

    message should matchPattern { case PlayError(_) => }
    currentPlayer.name should be(players.head)
  }

  it should "force the player to target themself if every other player is eliminated or protected" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // use this randomizer for tests that need specific card
    val stackedDeckRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, Prince, Guard) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    //player 1 will get a prince, player 2 a guard
    val game = Game.startMatch(Some(players.head))(stackedDeckRandomizer).exec(Game(players))

    def protectOrEliminateEveryoneThenPlay = for {
      _ <- Game.protectPlayer(players(1), isProtected = true)
      _ <- Game.protectPlayer(players(2), isProtected = true)
      _ <- Game.protectPlayer(players(3), isProtected = true)
      result <- Game.processTurn(players.head, Prince, None, None)(stackedDeckRandomizer)
      p2 <- Game.currentPlayer
      result2 <- Game.processTurn(players.head, Prince, Some(players.head), None)(stackedDeckRandomizer)
      p <- Game.currentPlayer
    } yield (p, result, result2, p2)

    val (newGame, (nextPlayer, result, result2, p2)) = protectOrEliminateEveryoneThenPlay(game)
    result should matchPattern { case PlayError(_) => }
    inside(result2) { case NextTurn(_, p) =>
      p should be("Kevin")
    }
    nextPlayer.name should be(players(1))
    p2.name should be(players.head)
    newGame.discard should contain only(Guard, Prince)

  }

  it should "fail if a player not in the game is targeted" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // use this randomizer for tests that need specific card
    val stackedDeckRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, Prince, Guard) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    //player 1 will get a prince, player 2 a guard
    val game = Game.startMatch(Some(players.head))(stackedDeckRandomizer).exec(Game(players))

    def playPrinceOnBadPlayer = for {
      p <- Game.currentPlayer
      result <- Game.processTurn(p.name, Prince, Some("BADPLAYER"), None)(stackedDeckRandomizer)
      currentPlayer <- Game.currentPlayer
    } yield (result, currentPlayer)

    val (newGame, (message, currentPlayer)) = playPrinceOnBadPlayer(game)

    message should matchPattern { case PlayError(_) => }
    currentPlayer.name should be(players.head)
    newGame should be(game)
  }

  behavior of "The king card"

  it should "switch the remaining card in the current player's hand with the target player" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // use this randomizer for tests that need specific card
    val stackedDeckRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, King, Priest, Guard, Guard, Handmaid) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    //player 1 will get a king and a handmaid, player 2 a priest
    val game = Game.startMatch(Some(players.head))(stackedDeckRandomizer).exec(Game(players))

    def playPrinceOnPlayer = for {
      p <- Game.currentPlayer
      result <- Game.processTurn(p.name, King, Some(players(1)), None)(stackedDeckRandomizer)
      kingPlayer <- Game.getPlayer(players.head)
      currentPlayer <- Game.currentPlayer
    } yield (result, kingPlayer.get, currentPlayer)

    val (newGame, (message, kingPlayer, currentPlayer)) = playPrinceOnPlayer(game)

    inside(message) { case NextTurn(_, p) =>
      p should be("Kevin")
    }
    currentPlayer.name should be(players(1))
    currentPlayer.hand should contain(Handmaid)
    kingPlayer.hand should contain only Priest
    newGame.discard should contain only King
  }

  it should "fail if the targeted player is protected" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // use this randomizer for tests that need specific card
    val stackedDeckRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, King, Priest, Guard, Guard, Handmaid) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    //player 1 will get a king and a handmaid, player 2 a priest
    val game = Game.startMatch(Some(players.head))(stackedDeckRandomizer).exec(Game(players))

    def playKingOnProtectedPlayer = for {
      _ <- Game.protectPlayer(players(1), isProtected = true)
      p <- Game.currentPlayer
      result <- Game.processTurn(p.name, King, Some(players(1)), None)(stackedDeckRandomizer)
      currentPlayer <- Game.currentPlayer
    } yield (result, currentPlayer)

    val (newGame, (message, currentPlayer)) = playKingOnProtectedPlayer(game)

    message should matchPattern { case PlayError(_) => }
    currentPlayer.name should be(players.head)
  }

  it should "fail if the targeted player is eliminated" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // use this randomizer for tests that need specific card
    val stackedDeckRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, King, Priest, Guard, Guard, Handmaid) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    //player 1 will get a king and a handmaid, player 2 a priest
    val game = Game.startMatch(Some(players.head))(stackedDeckRandomizer).exec(Game(players))

    def playKingOnEliminatedPlayer = for {
      _ <- Game.eliminatePlayer(players(1), isEliminated = true)
      p <- Game.currentPlayer
      result <- Game.processTurn(p.name, King, Some(players(1)), None)(stackedDeckRandomizer)
      currentPlayer <- Game.currentPlayer
    } yield (result, currentPlayer)

    val (message, currentPlayer) = playKingOnEliminatedPlayer.eval(game)

    message should matchPattern { case PlayError(_) => }
    currentPlayer.name should be(players.head)
  }

  it should "allow the player to play with no effect if every other player is eliminated or protected" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // use this randomizer for tests that need specific card
    val stackedDeckRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, King, Priest, Guard, Guard, Handmaid) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    //player 1 will get a king and a handmaid, player 2 a priest
    val game = Game.startMatch(Some(players.head))(stackedDeckRandomizer).exec(Game(players))

    def protectOrEliminateEveryoneThenPlay = for {
      _ <- Game.protectPlayer(players(1), isProtected = true)
      _ <- Game.eliminatePlayer(players(2), isEliminated = true)
      _ <- Game.protectPlayer(players(3), isProtected = true)
      result <- Game.processTurn(players.head, King, None, None)(stackedDeckRandomizer)
      p <- Game.currentPlayer
    } yield (p, result)

    val (nextPlayer, result) = protectOrEliminateEveryoneThenPlay.eval(game)
    inside(result) { case NextTurn(_, p) =>
      p should be("Kevin")
    }
    nextPlayer.name should be(players(1))

  }

  it should "fail if a player not in the game is targeted" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // use this randomizer for tests that need specific card
    val stackedDeckRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, King, Priest, Guard, Guard, Handmaid) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    //player 1 will get a king and a handmaid, player 2 a priest
    val game = Game.startMatch(Some(players.head))(stackedDeckRandomizer).exec(Game(players))

    def playKingOnBadplayer = for {
      p <- Game.currentPlayer
      result <- Game.processTurn(p.name, Priest, Some("BADPLAYER"), None)(stackedDeckRandomizer)
      currentPlayer <- Game.currentPlayer
    } yield (result, currentPlayer)

    val (newGame, (message, currentPlayer)) = playKingOnBadplayer(game)

    message should matchPattern { case PlayError(_) => }
    currentPlayer.name should be(players.head)
    newGame should be(game)
  }

  it should "fail if the player targets themself" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // use this randomizer for tests that need specific card
    val stackedDeckRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, King) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    //player 1 will get a King
    val game = Game.startMatch(Some(players.head))(stackedDeckRandomizer).exec(Game(players))
    val (newGame, result1) = Game.processTurn(players.head, King, Some(players.head), None)(stackedDeckRandomizer).apply(game)
    newGame should be(game)
    result1 should matchPattern { case PlayError(_) => }
  }

  behavior of "The countess card"

  it should "cause discarding a prince to fail" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // use this randomizer for tests that need specific card
    val stackedDeckRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, Countess, Priest, Guard, Guard, Prince) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    //player 1 will get a countess and a prince
    val game = Game.startMatch(Some(players.head))(stackedDeckRandomizer).exec(Game(players))

    def tryToDiscardPrince = for {
      result <- Game.processTurn(players.head, Prince, Some(players(1)), None)(stackedDeckRandomizer)
      p <- Game.currentPlayer
    } yield (p, result)

    val (nextPlayer, result) = tryToDiscardPrince.eval(game)
    result should matchPattern { case PlayError(_) => }
    nextPlayer.name should be(players.head)

  }

  it should "cause discarding a king to fail" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // use this randomizer for tests that need specific card
    val stackedDeckRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, Countess, Priest, Guard, Guard, King) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    //player 1 will get a countess and a king
    val game = Game.startMatch(Some(players.head))(stackedDeckRandomizer).exec(Game(players))

    def tryToDiscardKing = for {
      result <- Game.processTurn(players.head, King, Some(players(1)), None)(stackedDeckRandomizer)
      p <- Game.currentPlayer
    } yield (p, result)

    val (nextPlayer, result) = tryToDiscardKing.eval(game)
    result should matchPattern { case PlayError(_) => }
    nextPlayer.name should be(players.head)

  }

  it should "have no effect when discarded" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // use this randomizer for tests that need specific card
    val stackedDeckRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, Countess, Priest, Guard, Guard, Guard) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    //player 1 will get a countess and a guard
    val game = Game.startMatch(Some(players.head))(stackedDeckRandomizer).exec(Game(players))

    def discardCountess = for {
      result <- Game.processTurn(players.head, Countess, None, None)(stackedDeckRandomizer)
      p <- Game.currentPlayer
    } yield (p, result)

    val (nextPlayer, result) = discardCountess.eval(game)
    inside(result) { case NextTurn(_, p) =>
      p should be("Kevin")
    }
    nextPlayer.name should be(players(1))

  }

  behavior of "The princess card"

  it should "eliminate the player when discarded" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // use this randomizer for tests that need specific card
    val stackedDeckRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, Princess) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    //player 1 will get a princess
    val game = Game.startMatch(Some(players.head))(stackedDeckRandomizer).exec(Game(players))

    def discardPrincess = for {
      result <- Game.processTurn(players.head, Princess, None, None)(stackedDeckRandomizer)
      p <- Game.currentPlayer
      princessPlayer <- Game.getPlayer(players.head)
    } yield (p, princessPlayer.get, result)

    val (nextPlayer, princessPlayer, result) = discardPrincess.eval(game)
    inside(result) { case NextTurn(_, p) =>
      p should be("Kevin")
    }
    nextPlayer.name should be(players(1))
    princessPlayer.isEliminated should be(true)
  }

  it should "eliminate the player forced to discard it by the prince" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // use this randomizer for tests that need specific card
    val stackedDeckRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, Prince, Princess) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    //player 1 will get a prince, player 2 a princess
    val game = Game.startMatch(Some(players.head))(stackedDeckRandomizer).exec(Game(players))

    def forceDiscardOfPrincess = for {
      result <- Game.processTurn(players.head, Prince, Some(players(1)), None)(stackedDeckRandomizer)
      p <- Game.currentPlayer
      eliminatedPlayer <- Game.getPlayer(players(1))
    } yield (p, eliminatedPlayer.get, result)

    val (nextPlayer, eliminatedPlayer, result) = forceDiscardOfPrincess.eval(game)
    result should matchPattern { case NextTurn(_, "Morgan") => }
    nextPlayer.name should be(players(2))
    eliminatedPlayer.isEliminated should be(true)
  }

  it should "eliminate the player when they are forced to discard by their own prince" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // use this randomizer for tests that need specific card
    val stackedDeckRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, Prince, Prince, Prince, Prince, Princess) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    //player 1 will get a princess and a prince
    val game = Game.startMatch(Some(players.head))(stackedDeckRandomizer).exec(Game(players))

    def forceDiscardOfPrincess = for {
      _ <- Game.protectPlayer(players(1), isProtected = true)
      _ <- Game.protectPlayer(players(2), isProtected = true)
      _ <- Game.protectPlayer(players(3), isProtected = true)
      result <- Game.processTurn(players.head, Prince, Some(players.head), None)(stackedDeckRandomizer)
      p <- Game.currentPlayer
      eliminatedPlayer <- Game.getPlayer(players.head)
    } yield (p, eliminatedPlayer.get, result)

    val (nextPlayer, eliminatedPlayer, result) = forceDiscardOfPrincess.eval(game)
    inside(result) { case NextTurn(_, p) =>
      p should be("Kevin")
    }
    nextPlayer.name should be(players(1))
    eliminatedPlayer.isEliminated should be(true)
  }

  behavior of "the assassin card"

  it should "do nothing when discarded" in {
    val players = Seq("Tyler", "Kevin", "Morgan")
    // use this randomizer for tests that need specific card
    val stackedDeckRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, Assassin, Guard) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    //player 1 will get an assassin, player 2 will get a guard
    val game = Game.startMatch(Some(players.head))(stackedDeckRandomizer).exec(Game(players))

    def discardAssassin = for {
      result <- Game.processTurn(players.head, Assassin)(stackedDeckRandomizer)
    } yield result

    val (newGame, result) = discardAssassin(game)
    result should matchPattern { case NextTurn(_, _) => }
    newGame.players.map(p => (p.isEliminated, p.isProtected, p.score)) should contain theSameElementsAs game.players.map(p => (p.isEliminated, p.isProtected, p.score))
  }

  it should "eliminate a player who uses a guard on a player with this card" in {
    val players = Seq("Tyler", "Kevin", "Morgan")
    // use this randomizer for tests that need specific card
    val stackedDeckRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, Guard, Assassin) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    //player 1 will get an guard, player 2 will get a assassin
    val game = Game.startMatch(Some(players.head))(stackedDeckRandomizer).exec(Game(players))

    def playGuard = for {
      result <- Game.processTurn(players.head, Guard, Some("Kevin"), Some(Prince))(stackedDeckRandomizer)
    } yield result

    val (newGame, result) = playGuard(game)
    result should matchPattern { case NextTurn(_, _) => }
    newGame.players.find(_.name == "Tyler").get.isEliminated shouldBe true
  }

  it should "not eliminate the player with this card even if the guard guesses right" in {
    val players = Seq("Tyler", "Kevin", "Morgan")
    // use this randomizer for tests that need specific card
    val stackedDeckRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, Guard, Assassin) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    //player 1 will get an guard, player 2 will get a assassin
    val game = Game.startMatch(Some(players.head))(stackedDeckRandomizer).exec(Game(players))

    def playGuard = for {
      result <- Game.processTurn(players.head, Guard, Some("Kevin"), Some(Assassin))(stackedDeckRandomizer)
    } yield result

    val (newGame, result) = playGuard(game)
    result should matchPattern { case NextTurn(_, _) => }
    newGame.players.find(_.name == "Kevin").get.isEliminated shouldBe false
  }

  it should "be discarded if a guard is played on a player with this card" in {
    val players = Seq("Tyler", "Kevin", "Morgan")
    // use this randomizer for tests that need specific card
    val stackedDeckRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, Guard, Assassin, Guard, Priest, Princess) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    //player 1 will get an guard and a priest, player 2 will get a assassin. The next card in the draw pile will be a Princess
    val game = Game.startMatch(Some(players.head))(stackedDeckRandomizer).exec(Game(players))

    def playGuard = for {
      result <- Game.processTurn(players.head, Guard, Some("Kevin"), Some(Assassin))(stackedDeckRandomizer)
    } yield result

    val (newGame, result) = playGuard(game)
    result should matchPattern { case NextTurn(_, _) => }
    newGame.players.find(_.name == "Kevin").get.hand.head should be(Princess)
  }

  behavior of "The jester card"

  it should "award a point to the player who played jester if their target wins the match" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // use this randomizer for tests that need specific card
    val stackedDeckRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, Jester) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    //player 1 will get a jester
    val game = Game.startMatch(Some(players.head))(stackedDeckRandomizer).exec(Game(players))

    def targetPlayerThenMakeWinner = for {
      turnResult <- Game.processTurn(players.head, Jester, Some(players(2)), None)(stackedDeckRandomizer)
      _ <- Game.eliminatePlayer(players.head, isEliminated = true)
      _ <- Game.eliminatePlayer(players(1), isEliminated = true)
      _ <- Game.eliminatePlayer(players(3), isEliminated = true)
      winners <- Game.checkMatchOver(stackedDeckRandomizer)
    } yield (turnResult, winners)

    val (newGame, (turnResult, winners)) = targetPlayerThenMakeWinner(game)
    turnResult should matchPattern { case MatchOver(_, "Morgan", List("Tyler"), _) => }
    winners should equal(List(players.head, players(1)))
    newGame.players.find(_.name == players.head).get.score should be(1)
    newGame.players.find(_.name == players(1)).get.score should be(0)
    newGame.players.find(_.name == players(2)).get.score should be(1)
    newGame.players.find(_.name == players(3)).get.score should be(0)
  }

  it should "not award any extra points if the target does not win the match" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // use this randomizer for tests that need specific card
    val stackedDeckRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, Jester) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    //player 1 will get a jester
    val game = Game.startMatch(Some(players.head))(stackedDeckRandomizer).exec(Game(players))

    def targetPlayerThenMakeWinner = for {
      turnResult <- Game.processTurn(players.head, Jester, Some(players(1)), None)(stackedDeckRandomizer)
      _ <- Game.eliminatePlayer(players.head, isEliminated = true)
      _ <- Game.eliminatePlayer(players(1), isEliminated = true)
      _ <- Game.eliminatePlayer(players(3), isEliminated = true)
      winners <- Game.checkMatchOver(stackedDeckRandomizer)
    } yield (turnResult, winners)

    val (newGame, (turnResult, winners)) = targetPlayerThenMakeWinner(game)
    turnResult should matchPattern { case MatchOver(_, "Morgan", Nil, _) => }
    winners should equal(List(players.head))
    newGame.players.find(_.name == players.head).get.score should be(0)
    newGame.players.find(_.name == players(1)).get.score should be(0)
    newGame.players.find(_.name == players(2)).get.score should be(1)
    newGame.players.find(_.name == players(3)).get.score should be(0)
  }

  it should "not allow the player to target themself" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // use this randomizer for tests that need specific card
    val stackedDeckRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, Jester) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    //player 1 will get a jester
    val game = Game.startMatch(Some(players.head))(stackedDeckRandomizer).exec(Game(players))
    val (newGame, result1) = Game.processTurn(players.head, Jester, Some(players.head), None)(stackedDeckRandomizer).apply(game)
    newGame should be(game)
    result1 should matchPattern { case PlayError(_) => }
  }

  it should "fail if a target is not supplied" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // use this randomizer for tests that need specific card
    val stackedDeckRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, Jester) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    //player 1 will get a jester
    val game = Game.startMatch(Some(players.head))(stackedDeckRandomizer).exec(Game(players))
    val (newGame, result1) = Game.processTurn(players.head, Jester, None, None)(stackedDeckRandomizer).apply(game)
    newGame should be(game)
    result1 should matchPattern { case PlayError(_) => }
  }

  it should "fail if the targeted player is protected" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // use this randomizer for tests that need specific card
    val stackedDeckRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, Jester) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    //player 1 will get a jester
    val game = Game.startMatch(Some(players.head))(stackedDeckRandomizer).exec(Game(players))

    def protectThenTargetPlayer = for {
      _ <- Game.protectPlayer(players(1), isProtected = true)
      turnResult <- Game.processTurn(players.head, Jester, Some(players(1)), None)(stackedDeckRandomizer)
    } yield turnResult

    val (newGame, result1) = protectThenTargetPlayer(game)
    newGame should be(game)
    result1 should matchPattern { case PlayError(_) => }
  }

  it should "fail if the targeted player is eliminated" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // use this randomizer for tests that need specific card
    val stackedDeckRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, Jester) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    //player 1 will get a jester
    val game = Game.startMatch(Some(players.head))(stackedDeckRandomizer).exec(Game(players))

    def eliminateThenTargetPlayer = for {
      _ <- Game.eliminatePlayer(players(1), isEliminated = true)
      turnResult <- Game.processTurn(players.head, Jester, Some(players(1)), None)(stackedDeckRandomizer)
    } yield turnResult

    val (newGame, result1) = eliminateThenTargetPlayer(game)
    newGame should be(game)
    result1 should matchPattern { case PlayError(_) => }
  }

  it should "result in a tie if multiple players hit the point target at the end of a match" in (pending)

  behavior of "The dowager queen card"

  it should "eliminate the target player if your card is lower than theirs" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // use this randomizer for tests that need specific card
    val stackedDeckRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, DowagerQueen, Prince, Princess, Handmaid, Priest) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    //player 1 will get a dowager queen and a priest, player 2 a Prince
    val game = Game.startMatch(Some(players.head))(stackedDeckRandomizer).exec(Game(players))

    val (newGame, result) = Game.processTurn(players.head, DowagerQueen, Some(players(1)), None)(stackedDeckRandomizer).apply(game)

    result should matchPattern { case NextTurn(_, _) => }
    Game.getPlayer(players.head).eval(newGame).get.isEliminated should be(false)
    Game.getPlayer(players(1)).eval(newGame).get.isEliminated should be(true)
    Game.currentPlayer.eval(newGame).name should be(players(2))

  }

  it should "eliminate you if the target players card is lower than yours" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // use this randomizer for tests that need specific card
    val stackedDeckRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, DowagerQueen, Guard, Princess, Handmaid, Countess) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    //player 1 will get a dowager queen and a countess, player 2 a guard
    val game = Game.startMatch(Some(players.head))(stackedDeckRandomizer).exec(Game(players))

    val (newGame, result) = Game.processTurn(players.head, DowagerQueen, Some(players(1)), None)(stackedDeckRandomizer).apply(game)

    result should matchPattern { case NextTurn(_, _) => }
    Game.getPlayer(players.head).eval(newGame).get.isEliminated should be(true)
    Game.getPlayer(players(1)).eval(newGame).get.isEliminated should be(false)
    Game.currentPlayer.eval(newGame).name should be(players(1))

  }

  it should "eliminate nobody if you have the same card" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // use this randomizer for tests that need specific card
    val stackedDeckRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, DowagerQueen, Handmaid, Princess, Handmaid, Handmaid) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    //player 1 will get a dowager queen and a handmaid, player 2 a handmaid
    val game = Game.startMatch(Some(players.head))(stackedDeckRandomizer).exec(Game(players))

    val (newGame, result) = Game.processTurn(players.head, DowagerQueen, Some(players(1)), None)(stackedDeckRandomizer).apply(game)

    result should matchPattern { case NextTurn(_, _) => }
    Game.getPlayer(players.head).eval(newGame).get.isEliminated should be(false)
    Game.getPlayer(players(1)).eval(newGame).get.isEliminated should be(false)
    Game.currentPlayer.eval(newGame).name should be(players(1))

  }

  it should "fail if a player not in the game is targeted" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // use this randomizer for tests that need specific card
    val stackedDeckRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, DowagerQueen, Guard, Princess, Handmaid, Prince) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    //player 1 will get a dowager queen and a prince, player 2 a guard
    val game = Game.startMatch(Some(players.head))(stackedDeckRandomizer).exec(Game(players))

    def playDowagerQueenOnBadPlayer = for {
      p <- Game.currentPlayer
      result <- Game.processTurn(p.name, DowagerQueen, Some("BADPLAYER"), None)(stackedDeckRandomizer)
      currentPlayer <- Game.currentPlayer
    } yield (result, currentPlayer)

    val (newGame, (message, currentPlayer)) = playDowagerQueenOnBadPlayer(game)

    message should matchPattern { case PlayError(_) => }
    currentPlayer.name should be(players.head)
    newGame should be(game)
  }

  it should "fail if the targeted player is protected" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // use this randomizer for tests that need specific card
    val stackedDeckRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, DowagerQueen, Guard, Princess, Handmaid, Prince) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    //player 1 will get a dowagerQueen and a prince, player 2 a guard
    val game = Game.startMatch(Some(players.head))(stackedDeckRandomizer).exec(Game(players))

    def playDowagerQueenOnProtectedPlayer = for {
      _ <- Game.protectPlayer(players(1), isProtected = true)
      p <- Game.currentPlayer
      result <- Game.processTurn(p.name, DowagerQueen, Some(players(1)), None)(stackedDeckRandomizer)
      currentPlayer <- Game.currentPlayer
    } yield (result, currentPlayer)

    val (message, currentPlayer) = playDowagerQueenOnProtectedPlayer.eval(game)

    message should matchPattern { case PlayError(_) => }
    currentPlayer.name should be(players.head)
  }

  it should "fail if the targeted player is eliminated" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // use this randomizer for tests that need specific card
    val stackedDeckRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, DowagerQueen, Guard, Princess, Handmaid, Prince) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    //player 1 will get a dowager queen and a prince, player 2 a guard
    val game = Game.startMatch(Some(players.head))(stackedDeckRandomizer).exec(Game(players))

    def playDowagerQueenOnEliminatedPlayer = for {
      _ <- Game.eliminatePlayer(players(1), isEliminated = true)
      p <- Game.currentPlayer
      result <- Game.processTurn(p.name, DowagerQueen, Some(players(1)), None)(stackedDeckRandomizer)
      currentPlayer <- Game.currentPlayer
    } yield (result, currentPlayer)

    val (message, currentPlayer) = playDowagerQueenOnEliminatedPlayer.eval(game)

    message should matchPattern { case PlayError(_) => }
    currentPlayer.name should be(players.head)
  }

  it should "allow the player to play with no effect if every other player is eliminated or protected" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // use this randomizer for tests that need specific card
    val stackedDeckRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, DowagerQueen, Guard, Princess, Handmaid, Prince) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    //player 1 will get a dowager queen and a prince, player 2 a guard
    val game = Game.startMatch(Some(players.head))(stackedDeckRandomizer).exec(Game(players))

    def protectOrEliminateEveryoneThenPlay = for {
      _ <- Game.protectPlayer(players(1), isProtected = true)
      _ <- Game.eliminatePlayer(players(2), isEliminated = true)
      _ <- Game.protectPlayer(players(3), isProtected = true)
      result <- Game.processTurn(players.head, DowagerQueen, None, None)(stackedDeckRandomizer)
      p <- Game.currentPlayer
    } yield (p, result)

    val (nextPlayer, result) = protectOrEliminateEveryoneThenPlay.eval(game)
    inside(result) { case NextTurn(_, p) =>
      p should be("Kevin")
    }
    nextPlayer.name should be(players(1))

  }

  it should "fail if a target is not supplied" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // use this randomizer for tests that need specific card
    val stackedDeckRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, DowagerQueen, Guard, Princess, Handmaid, Prince) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    //player 1 will get a dowager queen and a prince, player 2 a guard
    val game = Game(players)
    val (game1, result1) = Game.processTurn(players.head, DowagerQueen, None, None)(stackedDeckRandomizer).apply(game)
    game1 should be(game)
    result1 should matchPattern { case PlayError(_) => }
  }

  it should "fail if the player targets themself" in {
    val players = Seq("Tyler", "Kevin", "Morgan", "Trevor")
    // use this randomizer for tests that need specific card
    val stackedDeckRandomizer = Randomizer(
      shuffleDeck = (s: Seq[Card]) => Seq(Guard, DowagerQueen) ++ s,
      choosePlayer = (s: Seq[Player]) => s.head
    )
    //player 1 will get a dowager queen
    val game = Game.startMatch(Some(players.head))(stackedDeckRandomizer).exec(Game(players))
    val (newGame, result1) = Game.processTurn(players.head, DowagerQueen, Some(players.head), None)(stackedDeckRandomizer).apply(game)
    newGame should be(game)
    result1 should matchPattern { case PlayError(_) => }
  }
}

