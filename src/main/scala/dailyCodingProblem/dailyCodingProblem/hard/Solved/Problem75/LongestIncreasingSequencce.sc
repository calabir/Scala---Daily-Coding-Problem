import org.junit.Assert.assertEquals

import scala.annotation.tailrec

/**
 * This problem was asked by Microsoft.
 *
 * Given an array of numbers, find the length of the longest increasing subsequence in the array. The subsequence does not necessarily have to be contiguous.
 *
 * For example, given the array [0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15], the longest increasing subsequence has length 6: it is 0, 2, 6, 9, 11, 15.
 */


val in1 = Array(0, 8, 4, 12, 2, 10, 6, 14, 1, 9, 5, 13, 3, 11, 7, 15)

//Dynamic programming + tailrec
@tailrec
def lisHelper0(in: Array[Int], i: Int, j: Int, lisIn: Array[Int]): Array[Int] = {
  if (j < i) {
    val newLisIn = if (in(i) > in(j)) lisIn.updated(i, math.max(lisIn(i), lisIn(j) + 1)) else lisIn
    lisHelper0(in, i, j + 1, newLisIn)
  } else {
    if (i < in.length - 1) lisHelper0(in, i + 1, 0, lisIn)
    else lisIn
  }
}
val arr0 = Array(3, 10, 2, 11)

def lis(in: Array[Int]): Int = {
  val lisIn = in.map(_ => 1)
  lisHelper0(in, 1, 0, lisIn).toList.max
}

assertEquals(3, lis(arr0))
assertEquals(6, lis(in1))

//TODO https://www.geeksforgeeks.org/longest-monotonically-increasing-subsequence-size-n-log-n/

//Recursive solution from GeekForGeeks https://www.geeksforgeeks.org/longest-increasing-subsequence-dp-3/
//var max_ref = 0
//def lisGfg(arr: Array[Int], n: Int): Int = {
//  if (n == 1) return 1
//  var res = 0
//  var max_ending_here = 1
//  for (i <- 1 until n) {
//    res = lisGfg(arr, i)
//    if (arr(i - 1) < arr(n - 1) && res + 1 > max_ending_here) max_ending_here = res + 1
//  }
//  if (max_ref < max_ending_here) max_ref = max_ending_here
//  max_ending_here
//}
//
//def lis(arr: Array[Int], n: Int) = {
//  max_ref = 1
//  lisGfg(arr, n)
//  max_ref
//}
//
//  val arr = Array(10, 22, 9, 33, 21, 50, 41, 60)
//  val n = arr.length
//
//  println("Length of lis is " + lis(arr, n) + "\n")
//  println("Length of lis is " + lis(in1, in1.length) + "\n")