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
  val description: String
  val requiresTarget: Boolean
  val requiresGuess: Boolean
  def doAction(discarder: Player, targetName: Option[String], guess: Option[Card]): State[Game, Either[String, String]] = State.state(Left("not yet implemented"))
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
  override def doAction(discarder: Player, targetName: Option[String], guess: Option[Card]): State[Game, Either[String, String]] = {
    require(targetName.isDefined)
    require(guess.isDefined)
    if(guess.get == Guard) {
      State.state(Left("You can't guess Guard"))
    } else {
      Game.getPlayer(targetName.get).flatMap(pOption => {
        pOption.map(p =>
          if(p.hand.contains(guess.get)) {
            Game.eliminatePlayer(p.name, true).map(_ => Right(s"You're right! $p.name is out"): Either[String, String])
          } else {
            State.state(Right(s"$p.name does not have a $guess.get.name"): Either[String, String]): State[Game, Either[String, String]]
          }
        ).getOrElse(State.state(Left(s"$targetName.get isn't in the game!"): Either[String, String]))
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
  override def doAction(discarder: Player, targetName: Option[String], guess: Option[Card] = None): State[Game, Either[String, String]] = State.state(Left("not yet implemented"))
}

case object Baron extends Card {
  val value = 3
  val name = "Baron"
  val description = "You and another player secretly compare hands. The player with the lower value is out of the round."
  val requiresTarget: Boolean = true
  val requiresGuess: Boolean = false
  override def doAction(discarder: Player, targetName: Option[String], guess: Option[Card] = None): State[Game, Either[String, String]] = State.state(Left("not yet implemented"))
}

case object Handmaid extends Card {
  val value = 4
  val name = "Handmaid"
  val description = "Until your next turn, ignore all effects from other player's cards."
  val requiresTarget: Boolean = false
  val requiresGuess: Boolean = false
  override def doAction(discarder: Player, targetName: Option[String] = None, guess: Option[Card] = None): State[Game, Either[String, String]] = State.state(Left("not yet implemented"))
}

case object Prince extends Card {
  val value = 5
  val name = "Prince"
  val description = "Choose any player (inluding yourself) ato discard his or her hand and draw a new card."
  val requiresTarget: Boolean = true
  val requiresGuess: Boolean = false
  override def doAction(discarder: Player, targetName: Option[String], guess: Option[Card] = None): State[Game, Either[String, String]] = State.state(Left("not yet implemented"))
}

case object King extends Card {
  val value = 6
  val name = "King"
  val description = "Trade hands with another player of your choice."
  val requiresTarget: Boolean = true
  val requiresGuess: Boolean = false
  override def doAction(discarder: Player, targetName: Option[String], guess: Option[Card] = None): State[Game, Either[String, String]] = State.state(Left("not yet implemented"))
}

case object Countess extends Card {
  val value = 7
  val name = "Countess"
  val description = "If you have this card and the King or Prince in your hand, you must discard this card."
  val requiresTarget: Boolean = false
  val requiresGuess: Boolean = false
  override def doAction(discarder: Player, targetName: Option[String] = None, guess: Option[Card] = None): State[Game, Either[String, String]] = State.state(Right("You discarded the Countess"))
}

case object Princess extends Card {
  val value = 8
  val name = "Princess"
  val description = "If you discard this card, you are out of the round."
  val requiresTarget: Boolean = false
  val requiresGuess: Boolean = false
  override def doAction(discarder: Player, targetName: Option[String] = None, guess: Option[Card] = None): State[Game, Either[String, String]] = State.state(Left("not yet implemented"))
}
