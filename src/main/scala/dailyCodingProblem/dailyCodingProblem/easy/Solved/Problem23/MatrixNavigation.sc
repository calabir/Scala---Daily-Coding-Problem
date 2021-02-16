import org.junit.Assert.assertEquals

/**
 * This problem was asked by Google.
 *
 * You are given an M by N matrix consisting of booleans that represents a board. Each True boolean represents a wall. Each False boolean represents a tile you can walk on.
 *
 * Given this matrix, a start coordinate, and an end coordinate, return the minimum number of steps required to reach the end coordinate from the start. If there is no possible path, then return null. You can move up, left, down, and right. You cannot move through walls. You cannot wrap around the edges of the board.
 *
 * For example, given the following board:
 *
 * [[f, f, f, f],
 * [t, t, f, t],
 * [f, f, f, f],
 * [f, f, f, f]]
 * and start = (3, 0) (bottom left) and end = (0, 0) (top left), the minimum number of steps required to reach the end is 7, since we would need to go through (1, 2) because there is a wall everywhere else on the second row.
 */

val matrix1 = Array(
  Array("f", "f", "f", "f"),
  Array("t", "t", "f", "t"),
  Array("f", "f", "f", "f"),
  Array("f", "f", "f", "f"))


//i = row, j= column
case class Position(i: Int, j: Int)

val start1 = Position(3, 0)
val end1 = Position(0, 0)

def isValid(start: Position, pos: Position, in: Array[Array[String]]): Position = {
  if ((pos.i >= 0 && pos.i < in.length && pos.j >= 0 && pos.j < in.length) && in(pos.i)(pos.j) != "t") {
    pos
  }else start
}

def up(in: Array[Array[String]], start: Position): Position = {
  val newPosition = Position(start.i - 1, start.j)
  isValid(start, newPosition, in)
}

assertEquals(Position(2, 0), up(matrix1, start1))
assertEquals(Position(2, 0), up(matrix1, Position(2, 0))) //due to the wall
assertEquals(Position(2, 3), up(matrix1, Position(3, 3)))
assertEquals(Position(0, 0), up(matrix1, Position(0, 0))) //can't wrap across the edges

def down(in: Array[Array[String]], start: Position): Position = {
  val newPosition = Position(start.i + 1, start.j)
  isValid(start, newPosition, in)
}

assertEquals(Position(3, 0), down(matrix1, start1)) //can't wrap across the edges
assertEquals(Position(0, 0), down(matrix1, Position(0, 0))) //due to the wall
assertEquals(Position(3, 2), down(matrix1, Position(2, 2)))


def left(in: Array[Array[String]], start: Position): Position = {
  val newPosition = Position(start.i, start.j - 1)
  isValid(start, newPosition, in)
}
assertEquals(Position(2, 1), left(matrix1, Position(2, 2)))
assertEquals(Position(3, 0), left(matrix1, Position(3, 0))) //can't wrap across the edges
assertEquals(Position(1, 2), left(matrix1, Position(1, 2))) //due to the wall


def right(in: Array[Array[String]], start: Position): Position = {
  val newPosition = Position(start.i, start.j + 1)
  isValid(start, newPosition, in)
}

assertEquals(Position(2, 3), right(matrix1, Position(2, 2)))
assertEquals(Position(2, 3), right(matrix1, Position(2, 3))) //can't wrap across the edges
assertEquals(Position(1, 2), right(matrix1, Position(1, 2))) //due to the wall


def nWays(in: Array[Array[String]], start: Position, end: Position, acc: Int, visited: List[Position]): List[Int] = {
  if (start == end) {
    List[Int](acc)
  }
  else {
    val leftPos = if (left(in, start) != start && !visited.contains(left(in, start)))
      nWays(in, left(in, start), end, acc + 1, visited :+ start) else List()
    val rightPos = if (right(in, start) != start && !visited.contains(right(in, start)))
      nWays(in, right(in, start), end, acc + 1, visited :+ start) else List()
    val downPos = if (down(in, start) != start && !visited.contains(down(in, start)))
      nWays(in, down(in, start), end, acc + 1, visited :+ start) else List()
    val upPos = if (up(in, start) != start && !visited.contains(up(in, start)))
      nWays(in, up(in, start), end, acc + 1, visited :+ start) else List()

    leftPos :++ rightPos :++ downPos :++ upPos
  }
}

def matrixNav(in: Array[Array[String]], start: Position, end: Position): Int = {
  nWays(in, start, end, 0, List()).min
}

assertEquals(7, matrixNav(matrix1, start1, end1))

