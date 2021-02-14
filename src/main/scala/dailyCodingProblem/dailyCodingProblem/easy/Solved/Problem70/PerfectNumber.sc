import org.junit.Assert.assertEquals

/**
 * This problem was asked by Microsoft.
 *
 * A number is considered perfect if its digits sum up to exactly 10.
 *
 * Given a positive integer n, return the n-th perfect number.
 *
 * For example, given 1, you should return 19. Given 2, you should return 28.
 */



def findPerfect(n: Int): Int = {
  require(n>0, "N should be positive")
  val perfectCompanion = (10-n).toString
  val ans = (n.toString + perfectCompanion).toInt
  ans
}

val in1 = 1
val ans1 = 19

assertEquals(ans1, findPerfect(in1))

val in2 = 2
val ans2 = 28

assertEquals(ans2, findPerfect(in2))