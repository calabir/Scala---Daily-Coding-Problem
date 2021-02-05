import junit.framework.Assert.assertEquals

/**
 * This problem was asked by Amazon.
 *
 * There exists a staircase with N steps, and you can climb up either 1 or 2 steps at a time. Given N, write a function that returns the number of unique ways you can climb the staircase. The order of the steps matters.
 *
 * For example, if N is 4, then there are 5 unique ways:
 *
 * 1, 1, 1, 1
 * 2, 1, 1
 * 1, 2, 1
 * 1, 1, 2
 * 2, 2
 * What if, instead of being able to climb 1 or 2 steps at a time, you could climb any number from a set of positive integers X? For example, if X = {1, 3, 5}, you could climb 1, 3, or 5 steps at a time.
 */

// First approximation, basically we set the list of ints that represents the staircase
// List(1,2,3,4) represents a staircase of 4 steps, we assume we can jump one or two steps every time
// Complexity is not good (is exponential -> O(n^2)
def uniqueWays(steps: List[Int]): Int = {
  steps match {
    case List() => 1
    case _ :: ys =>
      val twoSteps = if (ys.nonEmpty) uniqueWays(ys.tail) else 0
      uniqueWays(ys) + twoSteps
  }
}


//Also this problem is exactly the same as Fibonacci sequence
def fib(n: Int): Int = {
  if (n <= 1) n
  else fib(n - 1) + fib(n - 2)
}

fib(5)


def numWays0(s: Int): Int = {
  fib(s + 1)
}

val n = 4
val nList = 1 to 4 toList

numWays0(4)


//What about climbing a staircase of n steps jumping from a range of 1 to m steps at a time?
def countWaysUtil(n: Int, m: Int): Int = {
  if (n <= 1) return n
  var res = 0
  var i = 1
  while (i <= m && i <= n) {
    res += countWaysUtil(n - i, m)
    i += 1
  }
  res
}

// Returns number of ways to reach s'th stair hoping 1 to m times
def countWays(s: Int, m: Int) = countWaysUtil(s + 1, m)

countWays(4, 4)


//A more generic way
/**
 * Computes the number of ways to climb a staircaise of n steps hoping the times set in the list
 * @param n staircase size (steps)
 * @param list possible ways of hoping
 * @return the number of ways
 */
def nWays(n: Int, list: List[Int]): Int = {
  if (n < 0) 0
  else if (n == 0) 1
  else (for (i <- 1 to n) yield if (list.contains(i)) nWays(n - i, list) else 0).sum
}

assertEquals(0, nWays(4, List(5)))
assertEquals(1, nWays(4, List(1,5)))
assertEquals(5, nWays(4, List(1,2,5)))
assertEquals(7, nWays(4, List(1,2,3,5)))
assertEquals(0, nWays(4, List(3)))
assertEquals(1, nWays(3, List(3)))
assertEquals(0, nWays(3, List()))


//Extra ways - Dynamic Programming

def nWaysDP(n: Int): Int = {
  val res = new Array[Int](n + 1)
  res(0) = 1
  res(1) = 1
  res(2) = 2

  for (i <- 3 to n) yield res(i) = res(i - 1) + res(i - 2) + res(i - 3)

  res(n)

}

assertEquals(7, nWaysDP(4))

//TODO do a bottom up approach populating the Map instead of computing over and over the same values

val computedWays: Map[Int, Int] = Map()

def nWaysOpt(n: Int, list: List[Int]): Int = {
  if (n < 0) 0
  else if (n == 0) 1
  else (for (i <- 1 to n) yield if (list.contains(i)) nWays(n - i, list) else 0).sum
}