import org.junit.Assert.assertEquals

import scala.annotation.tailrec

/**
 * This problem was asked by Google.
 *
 * You are given an N by M 2D matrix of lowercase letters. Determine the minimum number of columns that can be removed to ensure that each row is ordered from top to bottom lexicographically. That is, the letter at each column is lexicographically later as you go down each row. It does not matter whether each row itself is ordered lexicographically.
 *
 * For example, given the following table:
 *
 * cba
 * daf
 * ghi
 * This is not ordered because of the a in the center. We can remove the second column to make it ordered:
 *
 * ca
 * df
 * gi
 * So your function should return 1, since we only needed to remove 1 column.
 *
 * As another example, given the following table:
 *
 * abcdef
 * Your function should return 0, since the rows are already ordered (there's only one row).
 *
 * As another example, given the following table:
 *
 * zyx
 * wvu
 * tsr
 * Your function should return 3, since we would need to remove all the columns to order it.
 * */



@tailrec
def checkOrderingHelper(inStr: String, in: Array[Array[String]], n: Int): Boolean = {
  if (n < in.length - 1)
    if (inStr < in(n)(0)) checkOrderingHelper(in(n)(0), in, n + 1) else false
  else if (inStr < in(n)(0)) true else false
}


def checkOrdering(in: Array[Array[String]]): Int = {
  if(in.length == 1) 0
  else{
    val orderedColumns = for (j <- in.indices) yield {
      val newIn = in.map(x => x.drop(j))
      checkOrderingHelper(newIn(0)(0), newIn, 1)
    }
    val numOfOrderedColumns = orderedColumns.map(x => if(x) 1 else 0).sum
    val columnsToRemove = in.head.length - numOfOrderedColumns
    columnsToRemove
  }
}
val in1 = Array(
  Array("c", "b", "a"),
  Array("d", "a", "f"),
  Array("g", "h", "i"))

assertEquals(1, checkOrdering(in1))

val in2 = Array(
  Array("a", "b", "c", "d", "e", "f"))

assertEquals(0, checkOrdering(in2))


val in3 = Array(
  Array("z", "y", "x"),
  Array("w", "v", "u"),
  Array("t", "s", "r"))

assertEquals(3, checkOrdering(in3))
