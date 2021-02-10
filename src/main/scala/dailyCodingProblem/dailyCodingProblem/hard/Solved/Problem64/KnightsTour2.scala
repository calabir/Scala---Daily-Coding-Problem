package dailyCodingProblem.dailyCodingProblem.hard.Solved.Problem64

object KnightsTour2 extends App {

  import org.junit.Assert.assertEquals

  /**
   *
   * @Version refactored from KnightsTour.scala
   * This problem was asked by Google.
   *
   * A knight's tour is a sequence of moves by a knight on a chessboard such that all squares are visited once.
   *
   * Given N, write a function to return the number of knight's tours on an N by N chessboard.
   *
   *
   * */


  val dimension_5 = 5
  val ways = computeKnightsTour(dimension_5)
//  computeKnightsTourDebug(dimension_5, 12)
  /**
   * Subtract n to x until the result is negative, it provides the row a cell belongs
   *
   * @param n dimension of the chess board
   * @param x cell value
   * @return the Int value representing the row where n is
   */
  def getRow(n: Int, x: Int): Int = {
    def getRowHelper(n: Int, x: Int, acc: Int): Int = {
      if (x - n >= 0) getRowHelper(n, x - n, acc + 1) else acc
    }

    getRowHelper(n, x, 0)
  }

  /**
   * Just the module between the cell's value and the dimension to know in which column it belongs
   *
   * @param n dimension of the chess board
   * @param x cell value
   * @return
   */
  def getCol(n: Int, x: Int): Int = {
    x % n
  }

  def getColAndRow(dimension: Int, xs: Int): (Int, Int) = {
    val column = getCol(dimension, xs)
    val row = getRow(dimension, xs)
    (column, row)
  }

  def nWays1(dimension: Int, acc: List[Int], debug: Boolean = false): Int = {
    if (debug) println("------------")
    if (debug) println("acc: " + acc)

    if (acc.length == dimension * dimension) {
//    if (acc.length == 2) {
      1
    } else {

      val xs = acc.last

      val (i: Int, j: Int) = getColAndRow(dimension, xs)

      if (debug) println("i: " + i)
      if (debug) println("j: " + j)

      val move1 = movement1(dimension, acc)
      val move2 = movement2(dimension, acc)
      val move3 = movement3(dimension, acc)
      val move4 = movement4(dimension, acc)
      val move5 = movement5(dimension, acc)
      val move6 = movement6(dimension, acc)
      val move7 = movement7(dimension, acc)
      val move8 = movement8(dimension, acc)

      move1 + move2 + move3 + move4 + move5 + move6 + move7 + move8
    }
  }

  def buildBoard(n: Int): List[Int] = {
    0 until (n * n) toList
  }

  def computeKnightsTour(n: Int): Int = {
    val boardValues = buildBoard(n)
    ((for (i <- boardValues) yield nWays1(n, List(i))) foldRight 0) (_ + _)
  }

  def computeKnightsTourDebug(n: Int, i: Int): Int = {
    nWays1(n, List(i))
  }

  private def movement8(dimension: Int, acc: List[Int]) = {
    val ys = acc.last
    val (i: Int, j: Int) = getColAndRow(dimension, ys)
    if (j - 2 >= 0 && i - 1 >= 0) {
      val newCell = ((j - 2) * dimension) + i - 1
      valueConstrain(dimension, acc, newCell)
    } else 0
  }


  private def movement7(dimension: Int, acc: List[Int]) = {
    val ys = acc.last
    val (i: Int, j: Int) = getColAndRow(dimension, ys)
    if (j - 2 >= 0 && i + 1 <= dimension - 1) {
      val newCell = ((j - 2) * dimension) + (i + 1)
      valueConstrain(dimension, acc, newCell)
    } else 0
  }

  private def movement6(dimension: Int, acc: List[Int]) = {
    val ys = acc.last
    val (i: Int, j: Int) = getColAndRow(dimension, ys)
    if (j + 2 <= dimension - 1 && i - 1 >= 0) {
      val newCell = ((j + 2) * dimension) + (i - 1)
      valueConstrain(dimension, acc, newCell)
    } else 0
  }

  private def movement5(dimension: Int, acc: List[Int]) = {
    val ys = acc.last
    val (i: Int, j: Int) = getColAndRow(dimension, ys)
    if (j + 2 <= dimension - 1 && i + 1 <= dimension - 1) {
      val newCell = ((j + 2) * dimension) + (i + 1)
      valueConstrain(dimension, acc, newCell)
    } else 0
  }

  private def movement4(dimension: Int, acc: List[Int]) = {

    val ys = acc.last
    val (i: Int, j: Int) = getColAndRow(dimension, ys)
    if (i - 2 >= 0 && j - 1 >= 0) {
      val newCell = ys - 2 - dimension
      valueConstrain(dimension, acc, newCell)
    } else 0
  }

  private def movement3(dimension: Int, acc: List[Int]) = {
    val ys = acc.last
    val (i: Int, j: Int) = getColAndRow(dimension, ys)
    if (i - 2 >= 0 && j + 1 <= dimension - 1) {
      val newCell = ys - 2 + dimension
      valueConstrain(dimension, acc, newCell)
    } else 0
  }


  private def movement2(dimension: Int, acc: List[Int]) = {
    val ys = acc.last
    val (i: Int, j: Int) = getColAndRow(dimension, ys)
    if (i + 2 <= dimension - 1 && j - 1 >= 0) {
      val newCell = ys + 2 - dimension //Subtract one J means: subtract a dimension's length to current val
      valueConstrain(dimension, acc, newCell)
    } else 0
  }

  private def movement1(dimension: Int, acc: List[Int]) = {
    val ys = acc.last
    val (i: Int, j: Int) = getColAndRow(dimension, ys)

    if (i + 2 <= dimension - 1 && j + 1 <= dimension - 1) {
      val newCell = ys + 2 + dimension //Add one J means: add a dimension's length to current val
      valueConstrain(dimension, acc, newCell)
    } else 0
  }

  private def valueConstrain(dimension: Int, acc: scala.List[Int], newCell: Int) = {
    if ((newCell > dimension * dimension - 1) || acc.contains(newCell)) 0
    else {
      nWays1(dimension, acc :+ newCell)
    }
  }

  println("Found: " + ways + " solutions in a " + dimension_5 + " board")

  // Some coverage tests

  assertEquals(2, getCol(4, 6))
  assertEquals(1, getCol(4, 9))

  assertEquals(1, getRow(4, 6))
  assertEquals(2, getRow(4, 9))

  assertEquals((0, 0), getColAndRow(4, 0))
  assertEquals((0, 1), getColAndRow(4, 4))
  assertEquals((3, 2), getColAndRow(4, 11))
  assertEquals((3, 3), getColAndRow(4, 15))

}
