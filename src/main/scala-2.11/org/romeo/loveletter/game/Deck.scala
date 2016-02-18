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
  def doAction(card: Card, discarder: Player, target: Option[Player], guess: Option[Card]): State[Game, Player] = ???
}

object Card {
  implicit def orderingByValue[A <: Card]: Ordering[A] =
    Ordering.by(a => a.value)
}

case object Guard extends Card {
  val value = 1
  val description = "Name a non-Guard card and choose another player. If that player has that card, he or she is out of the round."
  val requiresTarget: Boolean = true
  val requiresGuess: Boolean = true
  override def doAction(card: Card, discarder: Player, target: Option[Player], guess: Option[Card]): State[Game, Player] = ???
}

case object Priest extends Card {
  val value = 2
  val description = "Look at another player's hand."
  val requiresTarget: Boolean = true
  val requiresGuess: Boolean = false
  override def doAction(card: Card, discarder: Player, target: Option[Player], guess: Option[Card] = None): State[Game, Player] = ???
}

case object Baron extends Card {
  val value = 3
  val description = "You and another player secretly compare hands. The player with the lower value is out of the round."
  val requiresTarget: Boolean = true
  val requiresGuess: Boolean = false
  override def doAction(card: Card, discarder: Player, target: Option[Player], guess: Option[Card] = None): State[Game, Player] = ???
}

case object Handmaid extends Card {
  val value = 4
  val description = "Until your next turn, ignore all effects from other player's cards."
  val requiresTarget: Boolean = false
  val requiresGuess: Boolean = false
  override def doAction(card: Card, discarder: Player, target: Option[Player] = None, guess: Option[Card] = None): State[Game, Player] = ???
}

case object Prince extends Card {
  val value = 5
  val description = "Choose any player (inluding yourself) ato discard his or her hand and draw a new card."
  val requiresTarget: Boolean = true
  val requiresGuess: Boolean = false
  override def doAction(card: Card, discarder: Player, target: Option[Player], guess: Option[Card] = None): State[Game, Player] = ???
}

case object King extends Card {
  val value = 6
  val description = "Trade hands with another player of your choice."
  val requiresTarget: Boolean = true
  val requiresGuess: Boolean = false
  override def doAction(card: Card, discarder: Player, target: Option[Player], guess: Option[Card] = None): State[Game, Player] = ???
}

case object Countess extends Card {
  val value = 7
  val description = "If you have this card and the King or Prince in your hand, you must discard this card."
  val requiresTarget: Boolean = false
  val requiresGuess: Boolean = false
  override def doAction(card: Card, discarder: Player, target: Option[Player] = None, guess: Option[Card] = None): State[Game, Player] = ???
}

case object Princess extends Card {
  val value = 8
  val description = "If you discard this card, you are out of the round."
  val requiresTarget: Boolean = false
  val requiresGuess: Boolean = false
  override def doAction(card: Card, discarder: Player, target: Option[Player] = None, guess: Option[Card] = None): State[Game, Player] = ???
}
