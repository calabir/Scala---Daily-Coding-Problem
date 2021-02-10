import junit.framework.Assert.assertEquals

/**
 * This problem was asked by Amazon.
 *
 * Given an integer k and a string s, find the length of the longest substring that contains at most k distinct characters.
 *
 * For example, given s = "abcba" and k = 2, the longest substring with k distinct characters is "bcb".
 */


val k1 = 2
val in1 = "abcba"

val k2 = 3
val in2 = "abbbbcba"

def findLongest(in: String, k: Int, acc: String, accAux: String): String = {
  val newAcc = if (accAux.length > acc.length) accAux else acc
  if (in.isEmpty) acc
  else {
    if (accAux.contains(in.head)) {
      findLongest(in.tail, k, newAcc, accAux + in.head)
    }
    else if (k > 0) {
      findLongest(in.tail, k - 1, newAcc, accAux + in.head)
    } else {
      findLongest(in, k + 1, newAcc, accAux.drop(1))
    }
  }
}


assertEquals("bcb", findLongest(in1, k1, "", ""))
assertEquals("abbbbcb", findLongest(in2, k2, "", ""))