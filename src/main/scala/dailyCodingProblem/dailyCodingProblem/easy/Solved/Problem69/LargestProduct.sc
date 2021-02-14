import org.junit.Assert.assertEquals

/**
 * This problem was asked by Facebook.
 *
 * Given a list of integers, return the largest product that can be made by multiplying any three integers.
 *
 * For example, if the list is [-10, -10, 5, 2], we should return 500, since that's -10 * -10 * 5.
 *
 * You can assume the list has at least three integers.
 *
 * //https://www.geeksforgeeks.org/find-maximum-product-of-a-triplet-in-array/
 */

  //TODO do a more generic version of this, like using N integers instead of 3
val in1 = List(-10, -10, 5, 2)


//O(nlogn) Time, O(1) Space
def largestProduct(in: List[Int]): Int = {
  val inSorted = in.sorted

  val twoNegs = inSorted.take(2).product * inSorted.last
  val threePos = inSorted.takeRight(3).product

  Math.max(twoNegs, threePos)
}



assertEquals(500, largestProduct(in1))

