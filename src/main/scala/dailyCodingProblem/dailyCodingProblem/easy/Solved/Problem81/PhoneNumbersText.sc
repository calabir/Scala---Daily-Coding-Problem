import org.junit.Assert.assertEquals

import scala.annotation.tailrec

/**
 * This problem was asked by Yelp.
 *
 * Given a mapping of digits to letters (as in a phone number), and a digit string, return all possible letters the number could represent. You can assume each valid number in the mapping is a single digit.
 *
 * For example if {“2”: [“a”, “b”, “c”], 3: [“d”, “e”, “f”], …} then “23” should return [“ad”, “ae”, “af”, “bd”, “be”, “bf”, “cd”, “ce”, “cf"].
 * */

val mapping = Map[Char, List[String]](
  '2' -> List("a", "b", "c"),
  '3' -> List("d", "e", "f"),
  '4' -> List("g", "h", "i"),
  '5' -> List("j", "k", "l"),
  '6' -> List("m", "n", "o"),
  '7' -> List("p", "q", "r", "s"),
  '8' -> List("t", "u", "v"),
  '9' -> List("w", "x", "y", "z"))




//
//def expand(in: List[List[String]], acc: List[String]): List[String] = {
//  val res = if (in.length >= 2) {
//    val expandUntilOneLeft = (for (i <- in.head.indices; j <- in.head.indices) yield in.head(i) + in(1)(j)).toList
//    expand(in.drop(2), expandUntilOneLeft)
//  } else if (in.length == 1){
//    (for (i <- in.head.indices; j <- acc.indices) yield acc(j) + in.head(i)).toList
//  }else{
//    acc
//  }
//  res
//}

//Refactored to be more readable
@tailrec
def expand2(in: List[List[String]], acc: List[String]): List[String] = {
  in match {
    case List() => acc
    case xs::ys if ys.nonEmpty =>
      val newAcc = (for(i<-xs.indices; j<-ys.head.indices) yield xs(i) + ys.head(j)).toList
      expand2(in.drop(2), newAcc)
    case xs =>
      val newAcc = (for(i<-xs.head.indices; j<-acc.indices) yield acc(j) + xs.head(i)).toList
      expand2(in.drop(1), newAcc)
  }
}

def extractMapping(in: String): List[String] = {
  val inArray = in.toArray
  val expandIn = for (i <- List.range(0, inArray.length)) yield mapping(inArray(i))
  expand2(expandIn, List())
}

val in1 = "23"
val out1 = List("ad", "ae", "af", "bd", "be", "bf", "cd", "ce", "cf")

//assertEquals(out1,expand(in1, List()))
assertEquals(out1,extractMapping(in1))

val in2 = "234"
val out2 = List("adg", "aeg", "afg", "bdg", "beg", "bfg", "cdg", "ceg", "cfg", "adh", "aeh", "afh", "bdh", "beh", "bfh", "cdh", "ceh", "cfh", "adi", "aei", "afi", "bdi", "bei", "bfi", "cdi", "cei", "cfi")

//assertEquals(out2, expand(in2, List()))
assertEquals(out2, extractMapping(in2))



