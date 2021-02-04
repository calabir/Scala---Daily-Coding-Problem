package dailyCodingProblem.dailyCodingProblem.medium.Solved.Problem58

import org.junit.Assert.assertEquals

object RotatedSortedArray extends App {
  /**
   *
   * This problem was asked by Amazon.
   *
   * An sorted array of integers was rotated an unknown number of times.
   *
   * Given such an array, find the index of the element in the array in faster than linear time. If the element doesn't exist in the array, return null.
   *
   * For example, given the array [13, 18, 25, 2, 8, 10] and the element 8, return 4 (the index of 8 in the array).
   *
   * You can assume all the integers in the array are unique.
   */


  val in1 = Array(13, 18, 25, 2, 8, 10)

  def findTheIndex(input: Array[Int], e: Int): Int = {

    if (e == input(0)) return 0

    val splitInput = input.splitAt(input.length / 2)

    if (e >= input(0) && e <= input((input.length / 2) - 1)) {
      findTheIndex(splitInput._1, e)
    } else {
      input.length / 2 + findTheIndex(splitInput._2, e)
    }
  }

  assertEquals(0, findTheIndex(in1, 13))
  assertEquals(1, findTheIndex(in1, 18))
  assertEquals(2, findTheIndex(in1, 25))
  assertEquals(3, findTheIndex(in1, 2))
  assertEquals(4, findTheIndex(in1, 8))
  assertEquals(5, findTheIndex(in1, 10))

}
