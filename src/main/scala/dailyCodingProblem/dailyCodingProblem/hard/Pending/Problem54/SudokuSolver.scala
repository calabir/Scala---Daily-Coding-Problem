package dailyCodingProblem.dailyCodingProblem.hard.Pending.Problem54

/** This problem was asked by Dropbox.
 *
 * Sudoku is a puzzle where you're given a partially-filled 9 by 9 grid with digits. The objective is to fill the grid with the constraint that every row, column, and box (3 by 3 subgrid) must contain all of the digits from 1 to 9.
 *
 * Implement an efficient sudoku solver. */
//TODO unfinished, too long
object SudokuSolver extends App {
  val example1 = List(
    List(0, 0, 0, 0, 4, 5, 0, 8, 9),
    List(0, 4, 6, 0, 8, 0, 2, 0, 0),
    List(0, 5, 0, 2, 9, 0, 0, 0, 0),
    List(0, 2, 4, 7, 3, 8, 9, 0, 0),
    List(0, 0, 7, 9, 0, 0, 0, 0, 0),
    List(0, 0, 8, 0, 5, 2, 1, 7, 0),
    List(3, 0, 0, 0, 0, 6, 4, 0, 0),
    List(7, 0, 0, 0, 0, 0, 0, 1, 0),
    List(0, 0, 0, 8, 0, 0, 0, 3, 0)
  )

  //1 to 9 in every 3x3 square
  //1 to 9 in every row
  //1 to 9 in every column
  //No repeated values in any row, column or 3x3 square

  //example1.foreach(v => println(v))

  val shortExample = List(
    List(0, 1, 6),
    List(2, 0, 0),
    List(0, 4, 3))


  def check(in: List[List[Int]], col: Int, row: Int): List[Any] = {
    val allColValues = shortExample.map(v => v(col).toString).reduce(_ + _).toList.map(c => c.asDigit)
    val allRowValues = shortExample(row).map(v => v.toString).reduce(_ + _).toList.map(c => c.asDigit)
    val sudokuRange = 1 to 9 toList

    //Now we also need to identify the values that exist in the current 3x3 square
    val rowMod3 = row / 3
    val colMod3 = col / 3
    val inArr = in.toArray
    val squareValues = (for(i<- rowMod3 until rowMod3+3; j<-colMod3 until colMod3+3) yield inArr(i)(j)).distinct

    println("all missing values are: " + sudokuRange.filter(v => !allColValues.contains(v) && !allRowValues.contains(v) && !squareValues.contains(v)))

    sudokuRange.filter(v => !allColValues.contains(v) && !allRowValues.contains(v) && !squareValues.contains(v))
  }

  def computeWays(in: List[List[Int]]): List[(Int, List[List[Int]])] = {
//    in.map(v => check(v(1)))
  ???
  }

  case class sudokuValue(row: Int, col: Int, val value: Int)
//
//  shortExample.flatMap(l => {
//    val esto = for(i <- 1 to l.length) yield sudokuValue(i / 3, i/3, l(i-1) )
//    println(esto)
//    esto
//  })

  val algo = for(i <- shortExample.indices; j <- shortExample(i).indices) yield sudokuValue(i, j, shortExample(i)(j))

//  println("algo: " + algo)

  for(i <- algo) yield check(shortExample, i.row, i.col)

  //Now we Select the first field of the first list, and update all others (removing the value selected).
  //If some list becomes empty, and the value is not set, select the tail.head


  //fas un bucle per el primer list files
    //fas un bucle pel segon for columnes
}


//1,2,3 (1)
//4,5,6 (1+3)
//7,8,9 (1+3+3)