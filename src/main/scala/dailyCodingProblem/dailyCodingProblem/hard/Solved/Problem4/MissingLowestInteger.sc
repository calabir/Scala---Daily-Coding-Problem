import org.junit.Assert.assertEquals

import scala.annotation.tailrec

/**
 * This problem was asked by Stripe.
 *
 * Given an array of integers, find the first missing positive integer in linear time and constant space. In other words, find the lowest positive integer that does not exist in the array. The array can contain duplicates and negative numbers as well.
 *
 * For example, the input [3, 4, -1, 1] should give 2. The input [1, 2, 0] should give 3.
 *
 * You can modify the input array in-place.
 */


val array1 = Array(3, 4, -1, 1)
val array2 = Array(1, 2, 0)


def findTheMissingLowestInt(input: List[Int]): Int = {
  val min = input.min
  val max = input.max
  val set = min to max toSet

  val inputToSet = input.toSet


  @tailrec
  def findHelper(input: Set[Int], inputRange: List[Int], output: List[Int]): List[Int] = {
    inputRange match {
      case List() => output
      case xs :: ys => if (input.contains(xs)) findHelper(input, ys, output) else {
        findHelper(input,ys, output.appended(xs))}
    }
  }

  findHelper(inputToSet, set.toList.filter(_ > 0),List()).min
}

assertEquals(2, findTheMissingLowestInt(array1.toList))
assertEquals(3, findTheMissingLowestInt(array2.toList))