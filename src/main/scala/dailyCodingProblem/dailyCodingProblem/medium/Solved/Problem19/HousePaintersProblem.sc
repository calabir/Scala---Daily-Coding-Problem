import org.junit.Assert.assertEquals

/**
 * This problem was asked by Facebook.
 *
 * A builder is looking to build a row of N houses that can be of K different colors. He has a goal of minimizing cost while ensuring that no two neighboring houses are of the same color.
 *
 * Given an N by K matrix where the nth row and kth column represents the cost to build the nth house with kth color, return the minimum cost which achieves this goal.
 * */


//Rows = Colors
//Columns = Houses
//(Row,Column) = Cost

val matrix = Array(
  Array(1, 2, 3, 4),
  Array(4, 3, 2, 1),
  Array(4, 2, 1, 4),
  Array(1, 3, 2, 4))


def minHelper(in: Array[Array[Int]], n: Int): Int = {
  (for(i<-in.indices) yield in(i).drop(n).head).min
}

def minCost(in: Array[Array[Int]]): Int = {
  (for(j<-in.head.indices) yield minHelper(in, j)).sum
}

assertEquals(1+2+1+1, minCost(matrix))