import junit.framework.Assert.assertEquals

import scala.annotation.tailrec

/**
 * This problem was asked by Amazon.
 *
 * Given a string, find the longest palindromic contiguous substring. If there are more than one with the maximum length, return any one.
 *
 * For example, the longest palindromic substring of "aabcdcb" is "bcdcb". The longest palindromic substring of "bananas" is "anana".
 *
 */


@tailrec
def theLongestPalindrome(allCombs: List[String], acc: String): String = {
  allCombs match {
    case List() => acc
    case xs :: ys => if ((xs == xs.reverse) && xs.length > acc.length) theLongestPalindrome(ys, xs.reverse) else theLongestPalindrome(ys, acc)
  }
}

def wholeTree(word: String): String = {
  val letterList = (word foldRight ("")) ((x, y) => x + y).toList
  val letterList1: List[Char] = letterList.filter(c => letterList.tail.contains(c))

  val allWords = for {
    i <- 0 to letterList1.length
    j <- 1 to letterList1.length
  } yield letterList1.slice(i, i + j)

  val allCombs: Set[List[Char]] = allWords.filter(c => c.length > 1 && c.length <= word.length).toSet

  val allCombsString = allCombs.map(l => l.mkString(""))

  theLongestPalindrome(allCombsString.toList, "")

}

val letterList = ("Holo" foldRight "") ((x, y) => x + y).toList

assertEquals(wholeTree("Holo"), "olo") //5 - 25 -factor 5
assertEquals(wholeTree("amma"), "amma")
assertEquals(wholeTree("ammma"), "ammma")
assertEquals(wholeTree("Banana"), "anana")
assertEquals(wholeTree("aabcdcb"), "bcdcb")
assertEquals(wholeTree("amorroma"), "amorroma") // 8*9 = 72
//Complejidad WCS es O(n*(n+1))


"aabacde"
"Banana".length


def isPalindrome(word: String, acc: String, steps: Int): (String, Int) = {
  println(word)
  val reversedWord = word.reverse

  if((word == "") || word.length <= 1) (acc, steps)
  else if(word == reversedWord) {println("Found one: " +  reversedWord); (reversedWord, steps)}
  else (List(isPalindrome(word.take(word.length-steps), acc, steps + 1)._1 :+ isPalindrome(word.drop(steps), acc, steps + 1)._1).minBy(_.length).head.toString,1) //He de mirar el (take de length - steps) i el (drop de length - steps)
}

assertEquals(isPalindrome("Holo", "", 0)._1,"olo")
println("The number of steps for a length of " + "Holo".length + " was: " + isPalindrome("Holo", "", 0)._2)

assertEquals(isPalindrome("amma", "", 0)._1,"amma")
println("The number of steps for a length of " + "amma".length + " was: " + isPalindrome("amma", "", 0)._2)

assertEquals(isPalindrome("ammma", "", 0)._1,"ammma")
println("The number of steps for a length of " + "ammma".length + " was: " + isPalindrome("ammma", "", 0)._2)

assertEquals(isPalindrome("Banana", "", 0)._1,"anana")
println("The number of steps for a length of " + "Banana".length + " was: " + isPalindrome("Banana", "", 0)._2)

assertEquals(isPalindrome("aabcdcb", "", 0)._1,"bcdcb")
println("The number of steps for a length of " + "aabcdcb".length + " was: " + isPalindrome("aabcdcb", "", 0)._2)

assertEquals(isPalindrome("amorroma", "", 0)._1,"amorroma")
println("The number of steps for a length of " + "amorroma".length + " was: " + isPalindrome("amorroma", "", 0)._2)
