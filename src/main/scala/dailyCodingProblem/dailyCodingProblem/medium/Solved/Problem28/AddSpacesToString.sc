import org.junit.Assert.assertEquals

import scala.annotation.tailrec

/**
 * This problem was asked by Palantir.
 *
 * Write an algorithm to justify text. Given a sequence of words and an integer line length k, return a list of strings which represents each line, fully justified.
 *
 * More specifically, you should have as many words as possible in each line. There should be at least one space between each word. Pad extra spaces when necessary so that each line has exactly length k. Spaces should be distributed as equally as possible, with the extra spaces, if any, distributed starting from the left.
 *
 * If you can only fit one word on a line, then you should pad the right-hand side with spaces.
 *
 * Each word is guaranteed not to be longer than k.
 *
 * For example, given the list of words ["the", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog"] and k = 16, you should return the following:
 *
 * ["the  quick brown", # 1 extra space on the left
 * "fox  jumps  over", # 2 extra spaces distributed evenly
 * "the   lazy   dog"] # 4 extra spaces distributed evenly
 * */


val in1 = List("the", "quick", "brown", "fox", "jumps", "over", "the", "lazy", "dog")
val k1 = 16
val separator = " "

def identifyAllSpacesInString(in: String): List[Int] = {
  in.zipWithIndex.map(c => if (c._1 == separator.head) c._2 + 1 else 0).toList.filter(_ != 0)
}

@tailrec
def fillWithSpaces(in: String, k: Int, groupOfSpaces: Int, iteration: Int, allSpacesIndexes: List[Int]): String = {
  val initialSpaces = in.count(_ == separator.head)
  val newIn = if (initialSpaces == 0) {
    in :++ separator
  } else {
    val splitIn = in.splitAt(allSpacesIndexes(groupOfSpaces) + iteration)
    splitIn._1 :++ separator :++ splitIn._2
  }
  val (nextGroupOfSpaces, newIteration) = if (groupOfSpaces == allSpacesIndexes.length - 1) (0, iteration + 1) else (groupOfSpaces + 1, iteration)
  if (in.length == k) in
  else fillWithSpaces(newIn, k, nextGroupOfSpaces, newIteration, allSpacesIndexes)
}


def addSpaces(in: List[String], k: Int, acc: String, nWords: Int, resultAcc: List[String]): List[String] = {
  in match {
    case List() => List(fillWithSpaces(acc, k, 0, 0, identifyAllSpacesInString(acc))) //input list is empty but we need to expand
    case xs :: ys =>
      if (acc.length < k) {
        val newAcc = if (acc.isEmpty) xs else acc :++ separator :++ xs
        if (newAcc.length <= k) addSpaces(ys, k, newAcc, nWords, resultAcc) //we can add a new word
        else {
          val newAcc = fillWithSpaces(acc, k, 0, 0, identifyAllSpacesInString(acc))
          addSpaces(in, k, newAcc, nWords, resultAcc) //can't add a new word, but we can fill with spaces
        }
      } else {
        List(acc) :++ addSpaces(in, k, "", nWords, resultAcc)
      }
  }
}



assertEquals(List("the  quick brown","fox  jumps  over","the   lazy   dog"), addSpaces(in1, k1, "", 0, List()))