/**
 * This problem was recently asked by Google.
 *
 * Given a list of numbers and a number k, return whether any two numbers from the list add up to k.
 *
 * For example, given [10, 15, 3, 7] and k of 17, return true since 10 + 7 is 17.
 *
 * Bonus: Can you do this in one pass?
 */

val in = List(10, 15, 3, 7)
val k = 17

def computeSum(head: Int, j: Int, k: Int): Boolean = {
  if (head + j == k) true
  else false
}

def pairEqualsK(input: List[Int], k: Int): Boolean = {
  val result = for {
    i <- 0 to input.length
    j <- input.drop(i + 1)
  } yield computeSum(in.drop(i).head, j, k)

  result.contains(true)

}

pairEqualsK(in, k)


//Let's optimize it

val inSet = in.toSet

def checkerOpt(input: List[Int], set: Set[Int], k: Int): Boolean = {
  input match {
    case List() => false
    case _ => if (set.contains(k - input.head)) true else checkerOpt(input.tail, set, k)
  }
}

checkerOpt(in, inSet, k)