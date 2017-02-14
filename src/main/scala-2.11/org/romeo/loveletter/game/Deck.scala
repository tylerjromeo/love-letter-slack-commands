package org.romeo.loveletter.game

import scalaz.State
import Game._

object Deck {
  val cards: Seq[Card] = Seq.fill(5)(Guard) ++
    Seq.fill(2)(Priest) ++
    Seq.fill(2)(Baron) ++
    Seq.fill(2)(Handmaid) ++
    Seq.fill(2)(Prince) ++
    Seq.fill(1)(King) ++
    Seq.fill(1)(Countess) ++
    Seq.fill(1)(Princess)

  def getCardByName(name: String): Option[Card] = {
    cards.distinct.find(card => card.name.equalsIgnoreCase(name) || card.value.toString == name)
  }

  def isCardName(name: String): Boolean = getCardByName(name).isDefined
}

trait Card {
  val value: Int
  val name: String
  val description: String
  val requiresTarget: Boolean
  val requiresGuess: Boolean

  def doAction(discarder: Player, targetName: Option[String], guess: Option[Card]): State[Game, Either[String, Message]]

  override def toString: String =
    s"""$value: $name
       |$description""".stripMargin
}

object Card {
  implicit def orderingByValue[A <: Card]: Ordering[A] =
    Ordering.by(a => a.value)
}

case object Guard extends Card {
  val value = 1
  val name = "Guard"
  val description = "Name a non-Guard card and choose another player. If that player has that card, he or she is out of the round."
  val requiresTarget: Boolean = true
  val requiresGuess: Boolean = true

  override def doAction(discarder: Player, targetName: Option[String], guess: Option[Card]): State[Game, Either[String, Message]] = {
    if (targetName.isEmpty || guess.isEmpty) {
      return Game.isEveryoneElseProtectedOrEliminated.map(if (_) {
        Right(s"Everyone is safe, $name discarded with no effect")
      } else {
        Left(s"A target and guess must be specified")
      })
    }
    if (guess.get == Guard) {
      State.state(Left("You can't guess Guard"))
    } else {
      Game.getPlayer(targetName.get).flatMap(pOption => {
        pOption.map(p =>
          if (p.isProtected) {
            State.state[Game, Either[String, Message]](Left(s"${p.name} is protected"))
          } else if (p.isEliminated) {
            State.state[Game, Either[String, Message]](Left(s"${p.name} isn't in the match"))
          } else if (p.name == discarder.name) {
            State.state[Game, Either[String, Message]](Left(s"You can't target yourself with Guard"))
          } else if (p.hand.contains(guess.get)) {
            Game.eliminatePlayer(p.name, isEliminated = true).map(_ => Right(s"You're right! ${p.name} is out"): Either[String, Message])
          } else {
            State.state[Game, Either[String, Message]](Right(s"${p.name} does not have a ${guess.get.name}"))
          }
        ).getOrElse(State.state[Game, Either[String, Message]](Left(s"${targetName.get} isn't in the game!")))
      })
    }
  }
}

case object Priest extends Card {
  val value = 2
  val name = "Priest"
  val description = "Look at another player's hand."
  val requiresTarget: Boolean = true
  val requiresGuess: Boolean = false

  override def doAction(discarder: Player, targetName: Option[String], guess: Option[Card] = None): State[Game, Either[String, Message]] = {
    if (targetName.isEmpty) {
      return Game.isEveryoneElseProtectedOrEliminated.map(if (_) {
        Right(s"Everyone is safe, $name discarded with no effect")
      } else {
        Left(s"A target must be specified")
      })
    }
    Game.getPlayer(targetName.get).flatMap(pOption => {
      pOption.map(p =>
        if (p.isProtected) {
          State.state[Game, Either[String, Message]](Left(s"${p.name} is protected"))
        } else if (p.isEliminated) {
          State.state[Game, Either[String, Message]](Left(s"${p.name} isn't in the match"))
        } else if (p.name == discarder.name) {
          State.state[Game, Either[String, Message]](Left(s"You can't target yourself with Priest"))
        } else {
          State.state[Game, Either[String, Message]](Right(Private(s"${p.name} has a ${p.hand.head}")))
        }
      ).getOrElse(State.state[Game, Either[String, Message]](Left(s"${targetName.get} isn't in the game!")))
    })
  }
}

case object Baron extends Card {
  val value = 3
  val name = "Baron"
  val description = "You and another player secretly compare hands. The player with the lower value is out of the round."
  val requiresTarget: Boolean = true
  val requiresGuess: Boolean = false

  override def doAction(discarder: Player, targetName: Option[String], guess: Option[Card] = None): State[Game, Either[String, Message]] = {
    if (targetName.isEmpty) {
      return Game.isEveryoneElseProtectedOrEliminated.map(if (_) {
        Right(s"Everyone is safe, $name discarded with no effect")
      } else {
        Left(s"A target must be specified")
      })
    }
    Game.getPlayer(targetName.get).flatMap(pOption => {
      pOption.map(p => {
        if (p.isProtected) {
          State.state[Game, Either[String, Message]](Left(s"${p.name} is protected"))
        } else if (p.isEliminated) {
          State.state[Game, Either[String, Message]](Left(s"${p.name} isn't in the match"))
        } else if (p.name == discarder.name) {
          State.state[Game, Either[String, Message]](Left(s"You can't target yourself with Baron"))
        } else {
          val playerCard = discarder.hand.diff(Seq(Baron)).head
          //discard hasn't been processed yet, so remove the baron for the comparison
          val targetCard = p.hand.head
          if (targetCard.value > playerCard.value) {
            Game.eliminatePlayer(discarder.name, isEliminated = true).map(_ => Right(s"${discarder.name} has been eliminated and discards a ${playerCard.name}"): Either[String, Message])
          } else if (targetCard.value < playerCard.value) {
            Game.eliminatePlayer(p.name, isEliminated = true).map(_ => Right(s"${p.name} has been eliminated and discards a ${targetCard.name}"): Either[String, Message])
          } else {
            State.state[Game, Either[String, Message]](Right("It is a tie. No one is eliminated"))
          }
        }
      }).getOrElse(State.state[Game, Either[String, Message]](Left(s"${targetName.get} isn't in the game!")))
    })
  }
}

case object Handmaid extends Card {
  val value = 4
  val name = "Handmaid"
  val description = "Until your next turn, ignore all effects from other player's cards."
  val requiresTarget: Boolean = false
  val requiresGuess: Boolean = false

  override def doAction(discarder: Player, targetName: Option[String] = None, guess: Option[Card] = None): State[Game, Either[String, Message]] = {
    Game.protectPlayer(discarder.name, isProtected = true).map(_ => Right(s"${discarder.name} is protected")) //HAMMAID
  }
}

case object Prince extends Card {
  val value = 5
  val name = "Prince"
  val description = "Choose any player (including yourself) to discard his or her hand and draw a new card."
  val requiresTarget: Boolean = true
  val requiresGuess: Boolean = false
  val privateResponse: Boolean = false

  override def doAction(discarder: Player, targetName: Option[String], guess: Option[Card] = None): State[Game, Either[String, Message]] = {
    if (targetName.isEmpty) {
      return State.state[Game, Either[String, Message]](Left(s"A target must be specified"))
    }
    if (discarder.hand.contains(Countess)) {
      return State.state[Game, Either[String, Message]](Left(s"Can't discard $name with Countess"))
    }
    Game.getPlayer(targetName.get).flatMap(pOption => {
      pOption.map(p =>
        if (p.isProtected) {
          State.state[Game, Either[String, Message]](Left(s"${p.name} is protected"))
        } else if (p.isEliminated) {
          State.state[Game, Either[String, Message]](Left(s"${p.name} isn't in the match"))
        } else {
          //if you call this on yourself, you still have a prince in your hand, so remove that
          val cardToDiscard = (if (p.hand.length > 1) p.hand.diff(Seq(Prince)) else p.hand).head

          def discardThenDraw: State[Game, Either[String, Message]] = for {
            discard <- Game.playerDiscard(p.name, cardToDiscard)
            _ <- Game.drawFromDeckOrBurnCard(p.name)
          } yield Right(s"${p.name} forced to discard a ${discard.head.name}")

          def discardPrincess: State[Game, Either[String, Message]] = for {
            discard <- Game.playerDiscard(p.name, cardToDiscard)
            _ <- Game.eliminatePlayer(p.name, isEliminated = true)
          } yield Right(s"${p.name} forced to discard a ${discard.head}. ${p.name} is eliminated")

          if (Princess == cardToDiscard) discardPrincess else discardThenDraw
        }
      ).getOrElse(State.state[Game, Either[String, Message]](Left(s"${targetName.get} isn't in the game!")))
    })
  }
}

case object King extends Card {
  val value = 6
  val name = "King"
  val description = "Trade hands with another player of your choice."
  val requiresTarget: Boolean = true
  val requiresGuess: Boolean = false

  override def doAction(discarder: Player, targetName: Option[String], guess: Option[Card] = None): State[Game, Either[String, Message]] = {
    if (targetName.isEmpty) {
      return Game.isEveryoneElseProtectedOrEliminated.map {
        case true => Right(s"Everyone is safe, $name discarded with no effect")
        case false => Left(s"A target must be specified")
      }
    }
    if (discarder.hand.contains(Countess)) {
      return State.state(Left(s"Can't discard $name with Countess")): State[Game, Either[String, Message]]
    }
    Game.getPlayer(targetName.get).flatMap(pOption => {
      pOption.map(p => {
        if (p.isProtected) {
          State.state[Game, Either[String, Message]](Left(s"${p.name} is protected"))
        } else if (p.isEliminated) {
          State.state[Game, Either[String, Message]](Left(s"${p.name} isn't in the match"))
        } else if (p.name == discarder.name) {
          State.state[Game, Either[String, Message]](Left(s"You can't target yourself with King"))
        } else {
          val playerCard = discarder.hand.diff(Seq(King)).head
          //discard hasn't been processed yet, so remove the king for the comparison
          val targetCard = p.hand.head
          for {
            _ <- Game.updatePlayer(Some(p.copy(hand = Seq(playerCard))))
            _ <- Game.updatePlayer(Some(discarder.copy(hand = Seq(King, targetCard))))
          } yield Right(s"${discarder.name} switched hands with ${p.name}"): Either[String, Message]
        }
      }).getOrElse(State.state[Game, Either[String, Message]](Left(s"${targetName.get} isn't in the game!"): Either[String, Message]))
    })
  }
}

case object Countess extends Card {
  val value = 7
  val name = "Countess"
  val description = "If you have this card and the King or Prince in your hand, you must discard this card."
  val requiresTarget: Boolean = false
  val requiresGuess: Boolean = false

  override def doAction(discarder: Player, targetName: Option[String] = None, guess: Option[Card] = None): State[Game, Either[String, Message]] = {
    State.state(Right("You discarded the Countess"))
  }
}

case object Princess extends Card {
  val value = 8
  val name = "Princess"
  val description = "If you discard this card, you are out of the round."
  val requiresTarget: Boolean = false
  val requiresGuess: Boolean = false

  override def doAction(discarder: Player, targetName: Option[String] = None, guess: Option[Card] = None): State[Game, Either[String, Message]] = {
    Game.eliminatePlayer(discarder.name, isEliminated = true).map(_ => Right(s"${discarder.name} discarded a $name and is eliminated"): Either[String, Message])
  }
}

//PREMIUM cards

case object Jester extends Card {
  val value = 0
  val name = "Jester"
  val description = "If the chose player wins the round, gain an Affection Token."
  val requiresTarget: Boolean = false
  val requiresGuess: Boolean = false

  override def doAction(discarder: Player, targetName: Option[String] = None, guess: Option[Card] = None): State[Game, Either[Message, Message]] = ???
}

case object Assassin extends Card {
  val value = 0
  val name = "Assassin"
  val description = "A player using a Guard on you is out and you aren't. Discard and draw a new card."
  val requiresTarget: Boolean = false
  val requiresGuess: Boolean = false

  override def doAction(discarder: Player, targetName: Option[String] = None, guess: Option[Card] = None): State[Game, Either[Message, Message]] = ???
}

case object Cardinal extends Card {
  val value = 2
  val name = "Cardinal"
  val description = "Two players switch hands; you look at one."
  val requiresTarget: Boolean = false
  val requiresGuess: Boolean = false

  override def doAction(discarder: Player, targetName: Option[String] = None, guess: Option[Card] = None): State[Game, Either[Message, Message]] = ???
}

case object Baroness extends Card {
  val value = 3
  val name = "Baroness"
  val description = "Choose 1 or 2 players. Look at their hands."
  val requiresTarget: Boolean = false
  val requiresGuess: Boolean = false

  override def doAction(discarder: Player, targetName: Option[String] = None, guess: Option[Card] = None): State[Game, Either[Message, Message]] = ???
}

case object Sycophant extends Card {
  val value = 4
  val name = "Sycophant"
  val description = "Choose a player. The next card must choose them."
  val requiresTarget: Boolean = false
  val requiresGuess: Boolean = false

  override def doAction(discarder: Player, targetName: Option[String] = None, guess: Option[Card] = None): State[Game, Either[Message, Message]] = ???
}

case object Count extends Card {
  val value = 5
  val name = "Count"
  val description = "If in discard at the end of the round, add 1 to the number of the card in your hand."
  val requiresTarget: Boolean = false
  val requiresGuess: Boolean = false

  override def doAction(discarder: Player, targetName: Option[String] = None, guess: Option[Card] = None): State[Game, Either[Message, Message]] = ???
}

case object Constable extends Card {
  val value = 6
  val name = "Constable"
  val description = "If eliminated with this card in your discard pile, gan an Affection Token."
  val requiresTarget: Boolean = false
  val requiresGuess: Boolean = false

  override def doAction(discarder: Player, targetName: Option[String] = None, guess: Option[Card] = None): State[Game, Either[Message, Message]] = ???
}

case object DowagerQueen extends Card {
  val value = 7
  val name = "Dowager Queen"
  val description = "Compare hands with another player; higher number is out."
  val requiresTarget: Boolean = false
  val requiresGuess: Boolean = false

  override def doAction(discarder: Player, targetName: Option[String] = None, guess: Option[Card] = None): State[Game, Either[Message, Message]] = ???
}

case object Bishop extends Card {
  val value = 9
  val name = "Bishop"
  val description = "Guess a player's hand. Gain an Affection Token if correct. They may discard their hand and draw a new card."
  val requiresTarget: Boolean = false
  val requiresGuess: Boolean = false

  override def doAction(discarder: Player, targetName: Option[String] = None, guess: Option[Card] = None): State[Game, Either[Message, Message]] = ???
}
