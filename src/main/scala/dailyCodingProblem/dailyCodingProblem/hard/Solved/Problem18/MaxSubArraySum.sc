/**
 * Given an array of integers and a number k, where 1 <= k <= length of the array, compute the maximum values of each subarray of length k.
 *
 * For example, given array = [10, 5, 2, 7, 8, 7] and k = 3, we should get: [10, 7, 8, 8], since:
 *
 * 10 = max(10, 5, 2)
 * 7 = max(5, 2, 7)
 * 8 = max(2, 7, 8)
 * 8 = max(7, 8, 7)
 * Do this in O(n) time and O(k) space. You can modify the input array in-place and you do not need to store the results. You can simply print them out as you compute them.
 * */


val in1 = Array(10, 5, 2, 7, 8, 7)
val k1 = 3

//TODO there are ways to do it more complicated, but this works and complies the complexity goal

def findMaxValues(in: Array[Int], k: Int): Unit = {
  if(in.length >= k) {
    println(in.take(k).max)
    findMaxValues(in.drop(1), k)
  }
}

findMaxValues(in1, k1)