package dailyCodingProblem.dailyCodingProblem.hard.Solved.Problem64

object KnightsTour extends App {

  import org.junit.Assert.assertEquals

  /**
   * This problem was asked by Google.
   *
   * A knight's tour is a sequence of moves by a knight on a chessboard such that all squares are visited once.
   *
   * Given N, write a function to return the number of knight's tours on an N by N chessboard.
   *
   *
   * */


  val boardDimension = 8
  val dim5 = 5


  assertEquals(2, getCol(4, 6))
  assertEquals(1, getCol(4, 9))
  var maxAcc = 0

  assertEquals(1, getRow(4, 6))
  assertEquals(2, getRow(4, 9))
  var currentMaxAcc: List[Int] = List(0)

  assertEquals((0, 0), getColAndRow(4, 0))
  assertEquals((0, 1), getColAndRow(4, 4))
  assertEquals((3, 2), getColAndRow(4, 11))
  assertEquals((3, 3), getColAndRow(4, 15))

  def getCol(dimension: Int, xs: Int): Int = {
    if (xs >= dimension) xs - ((xs / dimension) * dimension) else xs
  }

  def getRow(dimension: Int, xs: Int): Int = {
    val col = getCol(dimension, xs)
    (xs - col) / dimension
  }

  def getColAndRow(dimension: Int, xs: Int): (Int, Int) = {
    (getCol(dimension, xs), getRow(dimension, xs))
  }

  def nWays1(dimension: Int, acc: List[Int]): Int = {
    //println("------------")
    //println("acc: " + acc)
    if (acc.length > maxAcc) {
      maxAcc = acc.length
      currentMaxAcc = acc
//      //println(acc)
//      //println(maxAcc)

    }
    //println(" <<<<<<<<<<<<<<<<<<<<< acc LENGTH IS: " + acc.length)
    if (acc.length == dimension * dimension) {
//    if (acc.length == 2) {
      //println("saliendo: " + acc)
      1
    } else {

      val xs = acc.last

      //* hem de cridar el nWays1 de tots els moviments possibles del knight, si no hi ha cap de disponible, calcular des de boardCells.tail
//      //println("xs: " + xs)
      val (i: Int, j: Int) = getColAndRow(dimension, xs)

//      //println("i: " + i)
//      //println("j: " + j)

      val iPlus2 = (i + 2)
      val iMinus2 = (i - 2)
      val iPlus1 = (i + 1)
      val iMinus1 = (i - 1)

      val jPlus2 = (j + 2)
      val jMinus2 = (j - 2)
      val jPlus1 = (j + 1)
      val jMinus1 = (j - 1)


      val move1 = if (iPlus2 <= dimension - 1 && jPlus1 <= dimension - 1) {
        //println("move1");
        val newCell = (xs + 2 + dimension) //Add one J means: add a dimension's length to current val
        //println("The new cell: " + newCell);
        if ((newCell > dimension * dimension - 1) || acc.contains(newCell)) 0
        else {
          ////println(acc :+ newCell)
          nWays1(dimension, acc :+ newCell)
        }
      } else 0
      val move2 = if (iPlus2 <= dimension - 1 && jMinus1 >= 0) {
        //println("move2");
        val newCell = (xs + 2 - dimension) //Subtract one J means: subtract a dimension's length to current val
        if ((newCell > dimension * dimension - 1) || acc.contains(newCell)) 0

        else {
          //println("The new cell: " + newCell)
          ////println(acc :+ newCell)
          nWays1(dimension, acc :+ newCell)
        }
      } else 0
      val move3 = if (iMinus2 >= 0 && jPlus1 <= dimension - 1) {
        //println("move3");
        val newCell = (xs - 2 + dimension)
        if ((newCell > dimension * dimension - 1) || acc.contains(newCell)) 0

        else {
          //println("The new cell: " + newCell);
          ////println(acc :+ newCell)
          nWays1(dimension, acc :+ newCell)
        }
      } else 0
      val move4 = if (iMinus2 >= 0 && jMinus1 >= 0) {
        //println("move4");
        val newCell = (xs - 2 - dimension)
        if ((newCell > dimension * dimension - 1) || acc.contains(newCell)) 0

        else {
          //println("The new cell: " + newCell);
          ////println(acc :+ newCell)
          nWays1(dimension, acc :+ newCell)
        }
      } else 0

      val move5 = if (jPlus2 <= dimension - 1 && iPlus1 <= dimension - 1) {
        //println("move5");
        val newCell = (jPlus2 * dimension + iPlus1)
        if ((newCell > dimension * dimension - 1) || acc.contains(newCell)) 0

        else {
          //println("The new cell: " + newCell)
          //println(acc :+ (newCell))
          nWays1(dimension, acc :+ (newCell))
        }
      } else 0
      val move6 = if (jPlus2 <= dimension - 1 && iMinus1 >= 0) {
        //println("move6");
        val newCell = (jPlus2 * dimension + iMinus1)
        if ((newCell > dimension * dimension - 1) || acc.contains(newCell)) 0

        else {
          //println("The new cell: " + (newCell))
          //println(acc :+ (jPlus2 * dimension + iMinus1))
          nWays1(dimension, acc :+ (newCell))
        }
      } else 0
      val move7 = if (jMinus2 >= 0 && iPlus1 <= dimension - 1) {
        //println("move7");
        val newCell = (jMinus2 * dimension + iPlus1)
        if ((newCell > dimension * dimension - 1) || acc.contains(newCell)) 0
        else {
          //println("The new cell: " + (newCell))
          //println(acc :+ (newCell))
          nWays1(dimension, acc :+ (newCell))
        }
      } else 0
      val move8 = if (jMinus2 >= 0 && iMinus1 >= 0) {
        //println("move8");
        val newCell = (jMinus2 * dimension + iMinus1)
        if ((newCell > dimension * dimension - 1) || acc.contains(newCell)) 0
        else {
          //println("The new cell: " + newCell)
          //println(acc :+ (newCell))
          nWays1(dimension, acc :+ (newCell))
        }
      } else 0
      move1 + move2 + move3 + move4 + move5 + move6 + move7 + move8
    }
  }
  val boardValues = 0 until (dim5*dim5) toList

 println( "Found: " + ((for (i <- boardValues) yield nWays1(dim5, List(i))) foldRight 0)(_ + _) + " solutions in a " + dim5 + " board")
}
