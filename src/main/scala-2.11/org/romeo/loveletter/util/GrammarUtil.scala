package org.romeo.loveletter.util

object GrammarUtil {

  def joinWithCommasPlusAnd(words: Seq[String]): String = {
    words.toList match {
      case Nil => ""
      case head :: Nil => head
      case item1 :: item2 :: Nil => s"$item1 and $item2"
      case wordsList => (wordsList.dropRight(1) :+ s"and ${wordsList.last}").mkString(", ")
    }
  }
}
