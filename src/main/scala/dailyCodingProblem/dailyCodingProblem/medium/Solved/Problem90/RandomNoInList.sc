import scala.annotation.tailrec

/**
 * This question was asked by Google.
 *
 * Given an integer n and a list of integers l, write a function that randomly generates a number from 0 to n-1 that isn't in l (uniform).
 * */

val in1 = List(2,4,6)
val n1 = 8

//Naive solution

@tailrec
def randN(n: Int, list: List[Int]): Int = {
  val randValue = ((Math.random() * n) +1).toInt
  if(list.contains(randValue)) randN(n, list) else randValue
}

//Improved version
def randN2(n: Int, list: List[Int]): Int = {
  val nList = (0 until n).toList
  val missingValuesInList = nList.diff(list)
  val randValue = (Math.random() * missingValuesInList.length).toInt
  missingValuesInList(randValue)
}
