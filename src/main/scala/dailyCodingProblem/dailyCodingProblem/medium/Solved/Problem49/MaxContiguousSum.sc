import org.junit.Assert.assertEquals

/** This problem was asked by Amazon.
 *
 * Given an array of numbers, find the maximum sum of any contiguous subarray of the array.
 *
 * For example, given the array [34, -50, 42, 14, -5, 86], the maximum sum would be 137, since we would take elements 42, 14, -5, and 86.
 *
 * Given the array [-5, -1, -8, -9], the maximum sum would be 0, since we would not take any elements.
 *
 * Do this in O(N) time.
 */

val in1 = Array(34, -50, 42, 14, -5, 86)
val in2 = Array(-5, -1, -8, -9)

def contSum(input: List[Int], acc: Int): Int = {
  input match {
    case List() => acc
    case _ =>
      if (acc + input.head <= 0) {
        contSum(input.tail, 0)
      } else {
        contSum(input.tail, acc + input.head)
      }
  }
}

assertEquals(137, contSum(in1.toList, 0))
assertEquals(0, contSum(in2.toList, 0))