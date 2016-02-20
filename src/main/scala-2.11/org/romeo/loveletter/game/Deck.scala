package org.romeo.loveletter.game

import scalaz.State

object Deck {
  val cards = Seq.fill(5)(Guard) ++
    Seq.fill(2)(Priest) ++
    Seq.fill(2)(Baron) ++
    Seq.fill(2)(Handmaid) ++
    Seq.fill(2)(Prince) ++
    Seq.fill(1)(King) ++
    Seq.fill(1)(Countess) ++
    Seq.fill(1)(Princess)
}

trait Card {
  val value: Int
  val name: String
  val description: String
  val requiresTarget: Boolean
  val requiresGuess: Boolean
  val privateResponse: Boolean
  def doAction(discarder: Player, targetName: Option[String], guess: Option[Card]): State[Game, Either[String, String]]
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
  val privateResponse: Boolean = false
  override def doAction(discarder: Player, targetName: Option[String], guess: Option[Card]): State[Game, Either[String, String]] = {
    if(targetName.isEmpty || guess.isEmpty) {
      return Game.isEveryoneElseProtectedOrEliminated.map(if(_) {
          Right(s"Everyone is safe, $name discarded with no effect")
        } else {
          Left(s"A target and guess must be specified")
        })
    }
    if(guess.get == Guard) {
      State.state(Left("You can't guess Guard"))
    } else {
      Game.getPlayer(targetName.get).flatMap(pOption => {
        pOption.map(p =>
          if(p.isProtected) {
            State.state(Left(s"${p.name} is protected")): State[Game, Either[String, String]]
          } else if(p.isEliminated) {
            State.state(Left(s"${p.name} isn't in the match")): State[Game, Either[String, String]]
          } else if(p.hand.contains(guess.get)) {
            Game.eliminatePlayer(p.name, true).map(_ => Right(s"You're right! ${p.name} is out"): Either[String, String])
          } else {
            State.state(Right(s"${p.name} does not have a ${guess.get.name}"): Either[String, String]): State[Game, Either[String, String]]
          }
        ).getOrElse(State.state(Left(s"${targetName.get} isn't in the game!"): Either[String, String]))
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
  val privateResponse: Boolean = true
  override def doAction(discarder: Player, targetName: Option[String], guess: Option[Card] = None): State[Game, Either[String, String]] = {
    if(targetName.isEmpty) {
      return Game.isEveryoneElseProtectedOrEliminated.map(if(_) {
          Right(s"Everyone is safe, $name discarded with no effect")
        } else {
          Left(s"A target must be specified")
        })
    }
    Game.getPlayer(targetName.get).flatMap(pOption => {
      pOption.map(p =>
          if(p.isProtected) {
            State.state(Left(s"${p.name} is protected")): State[Game, Either[String, String]]
          } else if(p.isEliminated) {
            State.state(Left(s"${p.name} isn't in the match")): State[Game, Either[String, String]]
          } else {
            State.state(Right(s"${p.name} has a ${p.hand.head}")): State[Game, Either[String, String]]
          }
      ).getOrElse(State.state(Left(s"${targetName.get} isn't in the game!"): Either[String, String]))
    })
  }
}

case object Baron extends Card {
  val value = 3
  val name = "Baron"
  val description = "You and another player secretly compare hands. The player with the lower value is out of the round."
  val requiresTarget: Boolean = true
  val requiresGuess: Boolean = false
  val privateResponse: Boolean = false
  override def doAction(discarder: Player, targetName: Option[String], guess: Option[Card] = None): State[Game, Either[String, String]] = {
    if(targetName.isEmpty) {
      return Game.isEveryoneElseProtectedOrEliminated.map(if(_) {
          Right(s"Everyone is safe, $name discarded with no effect")
        } else {
          Left(s"A target must be specified")
        })
    }
    Game.getPlayer(targetName.get).flatMap(pOption => {
      pOption.map(p => {
        if(p.isProtected) {
          State.state(Left(s"${p.name} is protected")): State[Game, Either[String, String]]
        } else if(p.isEliminated) {
          State.state(Left(s"${p.name} isn't in the match")): State[Game, Either[String, String]]
        } else {
          val playerCard = discarder.hand.diff(Seq(Baron)).head //discard hasn't been processed yet, so remove the baron for the comparison
          val targetCard = p.hand.head
          if(targetCard.value > playerCard.value) {
            Game.eliminatePlayer(discarder.name, true).map(_ => Right(s"${discarder.name} has been eliminated and discards a ${playerCard.name}")): State[Game, Either[String, String]]
          } else if(targetCard.value < playerCard.value) {
            Game.eliminatePlayer(p.name, true).map(_ => Right(s"${p.name} has been eliminated and discards a ${targetCard.name}")): State[Game, Either[String, String]]
          } else {
            State.state(Right("It is a tie. No one is eliminated")): State[Game, Either[String, String]]
          }
        }
      }).getOrElse(State.state(Left(s"${targetName.get} isn't in the game!"): Either[String, String]))
    })
  }
}

case object Handmaid extends Card {
  val value = 4
  val name = "Handmaid"
  val description = "Until your next turn, ignore all effects from other player's cards."
  val requiresTarget: Boolean = false
  val requiresGuess: Boolean = false
  val privateResponse: Boolean = false
  override def doAction(discarder: Player, targetName: Option[String] = None, guess: Option[Card] = None): State[Game, Either[String, String]] = {
    Game.protectPlayer(discarder.name, true).map(_ => Right(s"${discarder.name} is protected")) //HAMMAID
  }
}

case object Prince extends Card {
  val value = 5
  val name = "Prince"
  val description = "Choose any player (inluding yourself) ato discard his or her hand and draw a new card."
  val requiresTarget: Boolean = true
  val requiresGuess: Boolean = false
  val privateResponse: Boolean = false
  override def doAction(discarder: Player, targetName: Option[String], guess: Option[Card] = None): State[Game, Either[String, String]] = {
    if(targetName.isEmpty) {
      return State.state(Left(s"A target must be specified")): State[Game, Either[String, String]]
    }
    if(discarder.hand.contains(Countess)) {
      return State.state(Left(s"Can't discard $name with Countess")): State[Game, Either[String, String]]
    }
    Game.getPlayer(targetName.get).flatMap(pOption => {
      pOption.map(p =>
          if(p.isProtected) {
            State.state(Left(s"${p.name} is protected")): State[Game, Either[String, String]]
          } else if(p.isEliminated) {
            State.state(Left(s"${p.name} isn't in the match")): State[Game, Either[String, String]]
          } else {
            //if you call this on yourself, you still have a prince in your hand, so remove that
            val cardToDiscard = (if(p.hand.length > 1) p.hand.diff(Seq(Prince)) else p.hand).head
            def discardThenDraw: State[Game, Either[String, String]] = for {
              discard <- Game.playerDiscard(p.name, cardToDiscard)
              _ <- Game.drawCard(p.name)
            } yield Right(s"${p.name} forced to discard a ${discard.head}")
            def discardPrincess: State[Game, Either[String, String]] = for {
              discard <- Game.playerDiscard(p.name, cardToDiscard)
              _ <- Game.eliminatePlayer(p.name, true)
            } yield Right(s"${p.name} forced to discard a ${discard.head}. ${p.name} is eliminated")
            if(Princess == cardToDiscard) discardPrincess else discardThenDraw
          }
      ).getOrElse(State.state(Left(s"${targetName.get} isn't in the game!"): Either[String, String]))
    })
  }
}

case object King extends Card {
  val value = 6
  val name = "King"
  val description = "Trade hands with another player of your choice."
  val requiresTarget: Boolean = true
  val requiresGuess: Boolean = false
  val privateResponse: Boolean = false
  override def doAction(discarder: Player, targetName: Option[String], guess: Option[Card] = None): State[Game, Either[String, String]] = {
    if(targetName.isEmpty) {
      return Game.isEveryoneElseProtectedOrEliminated.map(if(_) {
          Right(s"Everyone is safe, $name discarded with no effect")
        } else {
          Left(s"A target must be specified")
        })
    }
    if(discarder.hand.contains(Countess)) {
      return State.state(Left(s"Can't discard $name with Countess")): State[Game, Either[String, String]]
    }
    Game.getPlayer(targetName.get).flatMap(pOption => {
      pOption.map(p => {
        if(p.isProtected) {
          State.state(Left(s"${p.name} is protected")): State[Game, Either[String, String]]
        } else if(p.isEliminated) {
          State.state(Left(s"${p.name} isn't in the match")): State[Game, Either[String, String]]
        } else {
          val playerCard = discarder.hand.diff(Seq(King)).head //discard hasn't been processed yet, so remove the king for the comparison
          val targetCard = p.hand.head
          for {
            _ <- Game.updatePlayer(Some(p.copy(hand = Seq(playerCard))))
            _ <- Game.updatePlayer(Some(discarder.copy(hand = Seq(King, targetCard))))
          } yield (Right(s"${discarder.name} switched hands with ${p.name}"): Either[String, String])
        }
      }).getOrElse(State.state(Left(s"${targetName.get} isn't in the game!"): Either[String, String]))
    })
  }
}

case object Countess extends Card {
  val value = 7
  val name = "Countess"
  val description = "If you have this card and the King or Prince in your hand, you must discard this card."
  val requiresTarget: Boolean = false
  val requiresGuess: Boolean = false
  val privateResponse: Boolean = false
  override def doAction(discarder: Player, targetName: Option[String] = None, guess: Option[Card] = None): State[Game, Either[String, String]] = {
    State.state(Right("You discarded the Countess"))
  }
}

case object Princess extends Card {
  val value = 8
  val name = "Princess"
  val description = "If you discard this card, you are out of the round."
  val requiresTarget: Boolean = false
  val requiresGuess: Boolean = false
  val privateResponse: Boolean = false
  override def doAction(discarder: Player, targetName: Option[String] = None, guess: Option[Card] = None): State[Game, Either[String, String]] = {
    Game.eliminatePlayer(discarder.name, true).map(_ => Right(s"${discarder.name} discarded a $name and is eliminated"): Either[String, String])
  }
}
