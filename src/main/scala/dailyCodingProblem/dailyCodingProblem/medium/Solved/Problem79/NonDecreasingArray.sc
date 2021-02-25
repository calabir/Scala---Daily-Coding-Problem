import org.junit.Assert.assertEquals

import scala.annotation.tailrec

/**
 * This problem was asked by Facebook.
 *
 * Given an array of integers, write a function to determine whether the array could become non-decreasing by modifying at most 1 element.
 *
 * For example, given the array [10, 5, 7], you should return true, since we can modify the 10 into a 1 to make the array non-decreasing.
 *
 * Given the array [10, 5, 1], you should return false, since we can't modify any one element to get a non-decreasing array.
 * */

//Assuming that given an Array[] arr[] from 0 to n-1. It's decreasing if all elements between 0<j<n-1 meet arr(j-1) > arr(j)

val in1 = Array(10, 5, 7)
val in2 = Array(10, 5, 1)


@tailrec
def becomeNonDecreasing(in: Array[Int], n: Int, acc: Seq[Int]): Any = {
  if (n < in.length && in(n) > in(n + 1)) becomeNonDecreasing(in.drop(n), n + 1, acc :+ in(n))
  else {
    if (acc.length > 1) false
    else true
  }
}


assertEquals(true, becomeNonDecreasing(in1, 0, Seq()))
assertEquals(false, becomeNonDecreasing(in2, 0, Seq()))


//Now we consider an element as a single digit

def modifyOneDigit(in: Int, originalIn: Array[Int], n: Int): Boolean = {
  val inDigits = in.toString.length
  val originalDigits = originalIn(n).toString.length
  val totalTake = inDigits - 1
  //We can only modify one element, so if the length difference is gt 1, we can't make the number lesser
  if (inDigits - originalDigits > 1) false
  else {
    val dropIntermediates = for (i <- 1 until totalTake)
      yield if ((in.toString.take(i) :++ in.toString.takeRight(totalTake - i)).toInt < originalIn(n)) true else false
    if (in.toString.take(1).toInt < originalIn(n)
      || in.toString.takeRight(1).toInt < originalIn(n) || dropIntermediates.contains(true)) true
    else false
  }
}
modifyOneDigit(54, Array(40, 5, 7), 1)

@tailrec
def becomeNonDecreasing2(in: Array[Int], n: Int, acc: Map[Int, Int]): Any = {

  if (n < in.length && in(n) > in(n + 1)) becomeNonDecreasing2(in.drop(n), n + 1, acc + (in(n) -> n))
  else {
    if (acc.size > 1) false
    else {
      modifyOneDigit(acc.head._1, in, n)
    }
  }
}


assertEquals(true, becomeNonDecreasing2(in1, 0, Map()))
assertEquals(false, becomeNonDecreasing2(in2, 0, Map()))


val in3 = Array(100, 5, 1)
assertEquals(false, becomeNonDecreasing2(in3, 0, Map()))

val in4 = Array(123, 24, 25)
assertEquals(false, becomeNonDecreasing2(in3, 0, Map()))

val in5 = Array(1234, 246, 257)
assertEquals(true, becomeNonDecreasing2(in5, 0, Map()))

val in6 = Array(1234, 125, 126)
assertEquals(true, becomeNonDecreasing2(in6, 0, Map())) //124

val in7 = Array(1234, 135, 136)
assertEquals(true, becomeNonDecreasing2(in6, 0, Map())) //134


