import org.junit.Assert.assertEquals

import scala.annotation.tailrec

/**
 * This problem was asked by Apple.
 *
 * Suppose you have a multiplication table that is N by N. That is, a 2D array where the value at the i-th row and j-th column is (i + 1) * (j + 1) (if 0-indexed) or i * j (if 1-indexed).
 *
 * Given integers N and X, write a function that returns the number of times X appears as a value in an N by N multiplication table.
 *
 * For example, given N = 6 and X = 12, you should return 4, since the multiplication table looks like this:
 *
 * | 1 | 2 | 3 | 4 | 5 | 6 |
 *
 * | 2 | 4 | 6 | 8 | 10 | 12 |
 *
 * | 3 | 6 | 9 | 12 | 15 | 18 |
 *
 * | 4 | 8 | 12 | 16 | 20 | 24 |
 *
 * | 5 | 10 | 15 | 20 | 25 | 30 |
 *
 * | 6 | 12 | 18 | 24 | 30 | 36 |
 *
 * And there are 4 12's in the table.
 * */


//Since there is no restriction in terms of  Big-O. Let's brute force the solution
def matrixOfN(n: Int): List[Int] = {
  val rangeN = (1 to n).toList
  (for(i<-rangeN;j<-rangeN) yield i*j)
}

def filterMatrixOfN(n: Int, target: Int): Int = {
  matrixOfN(n).count(_ == target)
}

filterMatrixOfN(6, 12)

//The following solution is O(n)
@tailrec
def findDivisors(target: Int, n: Int, k: Int, acc: List[Int]): List[Int] = {
  val module = target % k
  val newAcc = if(module == 0 && (target / k) <= n) acc :+ k else acc

  k match {
    case 1 => if(target <= n) {
      println(acc:+1)
      acc :+ 1 }else {println(acc);acc}
    case _ => findDivisors(target, n, k-1, newAcc)
  }
}

def repetitions(target: Int, n: Int): Int = {
  findDivisors(target, n, n, List()).length
}
findDivisors(12, 6, 6, List())
findDivisors(4, 6, 6, List())

assertEquals(filterMatrixOfN(6, 4), repetitions(4, 6))
assertEquals(filterMatrixOfN(6, 12), repetitions(12, 6))
assertEquals(filterMatrixOfN(6, 20), repetitions(20, 6))
