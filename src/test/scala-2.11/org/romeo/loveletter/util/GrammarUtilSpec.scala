package org.romeo.loveletter.util

import org.romeo.loveletter.game.Game.{GameOver, MatchOver, NextTurn, PlayError}
import org.scalatest.Inside._
import org.scalatest._
import scalaz.State

import scala.language.postfixOps

class GrammarUtilSpec extends FlatSpec with Matchers {

  behavior of "joinWithCommasPlusAnd"

  it should "join a list of words with commas, and have the word and before the last one" in {
    val wordList = Seq("foo", "bar", "baz")
    GrammarUtil.joinWithCommasPlusAnd(wordList) shouldBe "foo, bar, and baz"
  }

  it should "join 2 words with 'and'" in {
    val wordList = Seq("foo", "bar")
    GrammarUtil.joinWithCommasPlusAnd(wordList) shouldBe "foo and bar"
  }

  it should "return a single word on its own" in {
    val wordList = Seq("foo")
    GrammarUtil.joinWithCommasPlusAnd(wordList) shouldBe "foo"
  }

  it should "return an empty string for an empty list" in {
    val wordList = Nil
    GrammarUtil.joinWithCommasPlusAnd(wordList) shouldBe ""
  }
}

