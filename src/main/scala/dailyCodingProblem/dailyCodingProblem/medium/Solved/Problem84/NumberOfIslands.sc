import org.junit.Assert.assertEquals

/**
 * This problem was asked by Amazon.
 *
 * Given a matrix of 1s and 0s, return the number of "islands" in the matrix. A 1 represents land and 0 represents water, so an island is a group of 1s that are neighboring whose perimeter is surrounded by water.
 *
 * For example, this matrix has 4 islands.
 *
 * 1 0 0 0 0
 * 0 0 1 1 0
 * 0 1 1 0 0
 * 0 0 0 0 0
 * 1 1 0 0 1
 * 1 1 0 0 1
 *
 * * */

val in1 = Array(
  Array(1, 0, 0, 0, 0),
  Array(0, 0, 1, 1, 0),
  Array(0, 1, 1, 0, 0),
  Array(0, 0, 0, 0, 0),
  Array(1, 1, 0, 0, 1),
  Array(1, 1, 0, 0, 1))


//TODO there was no performance restriction. It's not optimal due to the for.
// We are computing all over again the accumulator (a.k.a the DAGs), better with a tail recursion and reuse the Acc

def findGraphs(in: Array[Array[Int]], acc: List[(Int, Int)], i: Int, j: Int): List[(Int, Int)] = {
  if (in(i)(j) == 1 && !acc.contains((i, j))) {
    val newAcc = if (!acc.contains((i, j))) acc :+ ((i, j)) else acc

    val up = if (i == 0 || in(i-1)(j) == 0) List()
    else findGraphs(in, newAcc, i - 1, j)

    val down = if (i == in.length - 1 || in(i+1)(j) == 0) List()
    else findGraphs(in, newAcc, i + 1, j)

    val left = if (j == 0 || in(i)(j-1) == 0) List()
    else findGraphs(in, newAcc, i, j - 1)

    val right = if (j == in1(i).length - 1 || in(i)(j+1) == 0) List()
    else findGraphs(in, newAcc, i, j + 1)

    newAcc.appendedAll(up).appendedAll(down).appendedAll(left).appendedAll(right).sorted.distinct

  }else acc
}

  def nIslands(in: Array[Array[Int]]): Int = {
    (for (i <- in.indices.toList; j <- in(i).indices.toList)
      yield findGraphs(in, List(), i, j)).distinct.count(_.nonEmpty)
  }




assertEquals(4, nIslands(in1))

