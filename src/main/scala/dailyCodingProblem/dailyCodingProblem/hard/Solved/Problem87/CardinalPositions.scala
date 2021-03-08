package dailyCodingProblem.dailyCodingProblem.hard.Solved.Problem87

import junit.framework.TestCase.assertEquals

import scala.annotation.tailrec


/**
 * This problem was asked by Uber.
 *
 * A rule looks like this:
 *
 * A NE B
 *
 * This means this means point A is located northeast of point B.
 *
 * A SW C
 *
 * means that point A is southwest of C.
 *
 * Given a list of rules, check if the sum of the rules validate. For example:
 *
 * A N B
 * B NE C
 * C N A
 * does not validate, since A cannot be both north and south of C.
 *
 * A NW B
 * A N B
 * is considered valid.
 */
object CardinalPositions extends App {
  //TODO not space optimal but at least the rules can get as big as I want, The approach should be more graph focused instead of Array focused
  val board = Array(Array("", "", ""), Array("", "", ""), Array("", "", ""))

  def isBoardValid(board: Array[Array[String]]): Boolean = {

    val countOccurrences = board.flatten.map(x => (x, 1)).filter(x => x._1 != "")
      .groupBy(_._1)
      .view.mapValues(_.map(_._2).sum)
      .toSeq
      .maxBy(_._2)
    if (countOccurrences._2 == 1) {
      true
    } else {
      if (board.toList.map(x => if (x.contains(countOccurrences._1)) 1 else 0).sum == 1) true
      else false
    }
  }

  @tailrec
  def findElementInNestedArray(ruleIn: rule, element: String, board: Array[Array[String]], i: Int, j: Int): (Int, Int, String) = {
    if (j >= board(i).length - 1 && i < board.length - 1) findElementInNestedArray(ruleIn, element, board, i + 1, 0)
    else if (board(i)(j) == element) (i, j, element)
    else if (j < board(i).length - 1) findElementInNestedArray(ruleIn, element, board, i, j + 1)
    else findElementInNestedArray(ruleIn, ruleIn.second, board, 0, 0)
  }

  def reverseDirection(direction: String): String = {
    direction match {
      case "NE" => "SW"
      case "N" => "S"
      case "NW" => "SE"
      case "E" => "W"
      case "W" => "E"
      case "SE" => "NW"
      case "S" => "N"
      case "SW" => "NE"
    }
  }

  def applyRule(board: Array[Array[String]], ruleIn: rule): Array[Array[String]] = {
    //if two elements collide in the same cell, consider invalid
    val newBoard = if (board.flatMap(x => x.map(y => if (y == "") true else false)).foldRight(true)(_ && _)) {
      board.updated(1, Array("", ruleIn.second, ""))

    } else board
    val (i, j, e) = findElementInNestedArray(ruleIn, ruleIn.first, newBoard, 0, 0)

    val (relativeDirection, elem) = if (e == ruleIn.first) (reverseDirection(ruleIn.direction), ruleIn.second) else (ruleIn.direction, ruleIn.first)

    val newExpandedBoard = if (i == newBoard.length - 1 || i == 0 || j == newBoard(i).length - 1 || j == 0) {
      val newBoard = board.map(x => Array("") :++ x :++ Array(""))
      val vec1 = Vector.fill(newBoard.head.length)("").toArray
      Array(vec1) :++ newBoard :++ Array(vec1)
    } else newBoard

    val (i2, j2, _) = findElementInNestedArray(ruleIn, ruleIn.first, newExpandedBoard, 0, 0)

    relativeDirection match {
      case "NE" => newExpandedBoard.updated(i2 - 1, newExpandedBoard(i2 - 1).updated(j2 - 1, elem))
      case "N" => newExpandedBoard.updated(i2 - 1, newExpandedBoard(i2 - 1).updated(j2, elem))
      case "NW" => newExpandedBoard.updated(i2 - 1, newExpandedBoard(i2 - 1).updated(j2 + 1, elem))
      case "E" => newExpandedBoard.updated(i2, newExpandedBoard(i2).updated(j2 - 1, elem))
      case "W" => newExpandedBoard.updated(i2, newExpandedBoard(i2).updated(j2 + 1, elem))
      case "SE" => newExpandedBoard.updated(i2 + 1, newExpandedBoard(i2 + 1).updated(j2 - 1, elem))
      case "S" => newExpandedBoard.updated(i2 + 1, newExpandedBoard(i2 + 1).updated(j2, elem))
      case "SW" => newExpandedBoard.updated(i2 + 1, newExpandedBoard(i2 + 1).updated(j2 + 1, elem))
    }
  }


  @tailrec
  def mainFunc(board: Array[Array[String]], rules: List[rule]): Boolean = {
    //    println("----------------")
    //    println("board: " + board.map(_.mkString(",")).mkString("Array(", ", ", ")"))
    rules match {
      case List() => isBoardValid(board)
      case xs :: ys => {
        val newBoard = applyRule(board, xs)
        mainFunc(newBoard, ys)
      }
    }
  }

  val in1 = List(rule("A", "N", "B"), rule("B", "NE", "C"), rule("C", "N", "A"))
  val in2 = List(rule("A", "NE", "B"), rule("A", "N", "B"))
  val in3 = List(rule("A", "N", "B"), rule("B", "N", "C"), rule("C", "N", "D"), rule("D", "N", "E"), rule("E", "N", "F"), rule("F", "N", "G"))


  assertEquals(true, mainFunc(board, in2))
  assertEquals(false, mainFunc(board, in1))
  assertEquals(true, mainFunc(board, in3))

  case class rule(first: String, direction: String, second: String)


}
