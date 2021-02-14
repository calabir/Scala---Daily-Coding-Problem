import org.junit.Assert.assertEquals

import scala.annotation.tailrec

/**
 * This problem was asked by Google.
 *
 * On our special chessboard, two bishops attack each other if they share the same diagonal. This includes bishops that have another bishop located between them, i.e. bishops can attack through pieces.
 *
 * You are given N bishops, represented as (row, column) tuples on a M by M chessboard. Write a function to count the number of pairs of bishops that attack each other. The ordering of the pair doesn't matter: (1, 2) is considered the same as (2, 1).
 *
 * For example, given M = 5 and the list of bishops:
 *
 * (0, 0),
 * (1, 2),
 * (2, 2),
 * (4, 0)
 * The board would look like this:
 *
 * [b 0 0 0 0]
 * [0 0 b 0 0]
 * [0 0 b 0 0]
 * [0 0 0 0 0]
 * [b 0 0 0 0]
 * You should return 2, since bishops 1 and 3 attack each other, as well as bishops 3 and 4.
 */



//TODO the four methods can be refactored to avoid having duplicated code

@tailrec
def leftDownDiagonal(in: (Int, Int), m: Int, acc: List[(Int, Int)]): List[(Int, Int)] = {
  if (in._1 < m - 1 && in._2 >= 0) {
    val newValue = (in._1 + 1, in._2 - 1)
    leftDownDiagonal(newValue, m, acc :+ newValue)
  } else {
    acc
  }
}

@tailrec
def leftUpDiagonal(in: (Int, Int), m: Int, acc: List[(Int, Int)]): List[(Int, Int)] = {
  if (in._2 > 0 && in._1 > 0) {
    val newValue = (in._1 - 1, in._2 - 1)
    leftUpDiagonal(newValue, m, acc :+ newValue)
  } else {
    acc
  }
}

@tailrec
def rightUpDiagonal(in: (Int, Int), m: Int, acc: List[(Int, Int)]): List[(Int, Int)] = {
  if (in._2 < m - 1 && in._1 > 0) {
    val newValue = (in._1 - 1, in._2 + 1)
    rightUpDiagonal(newValue, m, acc :+ newValue)
  } else {
    acc
  }
}

@tailrec
def rightDownDiagonal(in: (Int, Int), m: Int, acc: List[(Int, Int)]): List[(Int, Int)] = {
  if (in._2 < m - 1 && in._1 < m - 1) {
    val newValue = (in._1 + 1, in._2 + 1)
    rightDownDiagonal(newValue, m, acc :+ newValue)
  } else {
    acc
  }
}


@tailrec
def nWays(unmutedIn: List[(Int, Int)], in: List[(Int, Int)], m: Int, acc: List[(Int, Int)]): Int = {
  in match {
    case List() =>
      acc.map(x => if (unmutedIn.contains(x)) 1 else 0).sum / 2
    case xs :: ys =>
      val newAcc = acc :++ leftDownDiagonal(xs, m, List()) :++ leftUpDiagonal(xs, m, List()) :++ rightUpDiagonal(xs, m, List()) :++ rightDownDiagonal(xs, m, List())
      nWays(unmutedIn, ys, m, newAcc)
  }
}

val in1 = List((0, 0), (1, 2), (2, 2), (4, 0))
assertEquals(2,nWays(in1, in1, 5, List()))

val in2 = List((0, 0), (1, 1))
assertEquals(1,nWays(in2, in2, 2, List()))

val in3 = List((0, 0))
assertEquals(0,nWays(in3, in3, 2, List()))

