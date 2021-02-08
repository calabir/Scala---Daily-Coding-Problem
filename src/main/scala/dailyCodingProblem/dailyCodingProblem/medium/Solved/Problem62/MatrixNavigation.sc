import org.junit.Assert.assertEquals

/**
 * This problem was asked by Facebook.
 *
 * There is an N by M matrix of zeroes. Given N and M, write a function to count the number of ways of starting at the top-left corner and getting to the bottom-right corner. You can only move right or down.
 *
 * For example, given a 2 by 2 matrix, you should return 2, since there are two ways to get to the bottom-right:
 *
 * Right, then down
 * Down, then right
 * Given a 5 by 5 matrix, there are 70 ways to get to the bottom-right.
 */


val matrix1 = List(List(0))
val matrix2 = List(List(0, 0), List(0, 0))
val matrix5 = List(List(0, 0, 0, 0, 0),List(0, 0, 0, 0, 0), List(0, 0, 0, 0, 0), List(0, 0, 0, 0, 0), List(0, 0, 0, 0, 0))

//Right = 1, Down = 0, initial = -1, Both = -2, Nothing = -3
def nWays(matrix: List[List[Int]]): Int = {
  matrix match {
    case List(List(0)) => 1
    case xs::ys =>
      val goRight = if(xs.length > 1){ //If there is more than one element, we can go right
        val newMatrix = matrix.map(l => l.drop(1))
        nWays(newMatrix)
      } else 0
      val goDown = if(ys.nonEmpty){//If matrix has more than one element we can go down
        nWays(ys)
      } else 0
      goRight + goDown
  }
}


assertEquals(1, nWays(matrix1))
assertEquals(2, nWays(matrix2))
assertEquals(70, nWays(matrix5))

