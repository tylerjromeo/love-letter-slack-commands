package org.romeo.loveletter.game

object Deck {
  val cards = Seq.fill(5)(new Guard) ++
    Seq.fill(2)(new Priest) ++
    Seq.fill(2)(new Baron) ++
    Seq.fill(2)(new Handmaid) ++
    Seq.fill(2)(new Prince) ++
    Seq.fill(1)(new King) ++
    Seq.fill(1)(new Countess) ++
    Seq.fill(1)(new Princess)
}

trait Card {
  val value: Int
  val description: String
  //TODO: this interface needs to be able to deal with player inputs too... figure that out
  def doAction(state: Game): Unit = {}
}

class Guard extends Card {
  val value = 1
  val description = "Name a non-Guard card and choose another player. If that player has that card, he or she is out of the round."
}

class Priest extends Card {
  val value = 2
  val description = "Look at another player's hand."
}

class Baron extends Card {
  val value = 3
  val description = "You and another player secretly compare hands. The player with the lower value is out of the round."
}

class Handmaid extends Card {
  val value = 4
  val description = "Until your next turn, ignore all effects from other player's cards."
}

class Prince extends Card {
  val value = 5
  val description = "Choose any player (inluding yourself) ato discard his or her hand and draw a new card."
}

class King extends Card {
  val value = 6
  val description = "Trade hands with another player of your choice."
}

class Countess extends Card {
  val value = 7
  val description = "If you have this card and the King or Prince in your hand, you must discard this card."
}

class Princess extends Card {
  val value = 8
  val description = "If you discard this card, you are out of the round."
}
