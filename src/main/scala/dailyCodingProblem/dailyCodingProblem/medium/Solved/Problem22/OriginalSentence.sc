import org.junit.Assert.assertEquals

import scala.annotation.tailrec


/**
 * This problem was asked by Microsoft.
 *
 * Given a dictionary of words and a string made up of those words (no spaces), return the original sentence in a list.
 * If there is more than one possible reconstruction, return any of them. If there is no possible reconstruction, then return null.
 *
 * For example, given the set of words 'quick', 'brown', 'the', 'fox', and the string "thequickbrownfox", you should return ['the', 'quick', 'brown', 'fox'].
 *
 * Given the set of words 'bed', 'bath', 'bedbath', 'and', 'beyond', and the string "bedbathandbeyond", return either ['bed', 'bath', 'and', 'beyond] or ['bedbath', 'and', 'beyond'].
 * */


@tailrec
def findWord(in: String, dict: Set[String], n: Int): (String, Int) = {
  val takeN = in.take(n)
  if (dict(takeN)) (takeN, n)
  else findWord(in, dict, n + 1)
}


@tailrec
def originalSentence(in: String, dict: Set[String], acc: List[String]): List[String] = {
  in match {
    case empty if (in.length == 0) => acc
    case _ => {
      val (word, length) = findWord(in, dict, 0)
      originalSentence(in.drop(length), dict, acc :+ word)
    }
  }
}

val dictionary1 = Set("quick", "brown", "the", "fox")
val in1 = "thequickbrownfox"
val ans1 = List("the", "quick", "brown", "fox")

assertEquals("the", findWord(in1, dictionary1, 0)._1)

assertEquals(ans1, originalSentence(in1, dictionary1, List()))

val dictionary2 = Set("bed", "bath", "bedbath", "and", "beyond")
val in2 = "bedbathandbeyond"
val ans2_1 = List("bed", "bath", "and", "beyond")
//val ans2_2 = List("bedbath", "and", "beyond")

assertEquals(ans2_1, originalSentence(in2, dictionary2, List()))
