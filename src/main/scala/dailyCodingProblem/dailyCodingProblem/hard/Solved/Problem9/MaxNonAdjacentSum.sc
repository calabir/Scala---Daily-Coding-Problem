import org.junit.Assert.assertEquals

import scala.annotation.tailrec

/**
 * This problem was asked by Airbnb.
 *
 * Given a list of integers, write a function that returns the largest sum of non-adjacent numbers. Numbers can be 0 or negative.
 *
 * For example, [2, 4, 6, 2, 5] should return 13, since we pick 2, 6, and 5. [5, 1, 1, 5] should return 10, since we pick 5 and 5.
 *
 * Follow-up: Can you do this in O(N) time and constant space?
 *
 * This problem is the same as:
 * https://www.geeksforgeeks.org/find-maximum-possible-stolen-value-houses/
 * */



def newAccValue(acc: Array[Int], listValue: Int): Int = {
  if (acc.isEmpty) listValue
  else if (acc.length == 1) math.max(acc(0), listValue)
  else math.max(acc(acc.length - 2) + listValue, acc(acc.length - 1))
}

@tailrec
def myFindMaxSum(inList: List[Int], acc: Array[Int]): Int = {
  inList match {
    case List() => acc.last
    case xs :: ys =>
      myFindMaxSum(ys, acc :+ newAccValue(acc, xs))
  }
}

val inList1 = List(2, 4, 6, 2, 5)
val inList2 = List(5, 1, 1, 5)
val inList3 = List(5, -1, -1, 5)

val emptyArray = Array[Int]()

assertEquals(13, myFindMaxSum(inList1, emptyArray))
assertEquals(10, myFindMaxSum(inList2, emptyArray))
assertEquals(10, myFindMaxSum(inList3, emptyArray))

