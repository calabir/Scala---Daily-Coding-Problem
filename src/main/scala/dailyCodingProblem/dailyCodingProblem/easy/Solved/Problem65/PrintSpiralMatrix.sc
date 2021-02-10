import junit.framework.Assert.assertEquals

/**
 * This problem was asked by Amazon.
 *
 * Given a N by M matrix of numbers, print out the matrix in a clockwise spiral.
 *
 * For example, given the following matrix:
 *
 * [[1,  2,  3,  4,  5],
 * [6,  7,  8,  9,  10],
 * [11, 12, 13, 14, 15],
 * [16, 17, 18, 19, 20]]
 * You should print out the following:
 *
 * 1
 * 2
 * 3
 * 4
 * 5
 * 10
 * 15
 * 20
 * 19
 * 18
 * 17
 * 16
 * 11
 * 6
 * 7
 * 8
 * 9
 * 14
 * 13
 * 12
 */


def buildStringSpiralPrint(in: List[List[Int]], acc: List[Int], n: Int, m: Int): String = {
  in match {
    case List() => acc.mkString(",")
    case noTail if (in.tail.isEmpty) => acc.appendedAll(in.head).mkString(",")
    case xs :: ys => {
      val newAcc: List[Int] = acc.appendedAll(xs)
      val last = ys.last.reverse
      val newIn = in.dropRight(1).drop(1)
      val tails: List[Int] = newIn.map(l => l.last)
      val heads: List[Int] = newIn.map(l => l.head).reverse
      val newNewIn: List[List[Int]] = newIn.map(l => l.drop(1).dropRight(1))
      val finalAcc = newAcc.appendedAll(tails).appendedAll(last).appendedAll(heads)
      buildStringSpiralPrint(newNewIn, finalAcc, n, m)

    }
  }
}

val list1 = List(
  List(1, 2, 3, 4, 5),
  List(6, 7, 8, 9, 10),
  List(11, 12, 13, 14, 15),
  List(16, 17, 18, 19, 20))

val n = 5
val m = 4

assertEquals("1,2,3,4,5,10,15,20,19,18,17,16,11,6,7,8,9,14,13,12", buildStringSpiralPrint(list1, List(), n, m))

val list2 = List(
  List(1, 2, 3, 4, 5),
  List(6, 7, 8, 9, 10),
  List(11, 12, 13, 14, 15),
  List(16, 17, 18, 19, 20),
  List(21, 22, 23, 24, 25))

val n2 = 5
val m2 = 5

assertEquals("1,2,3,4,5,10,15,20,25,24,23,22,21,16,11,6,7,8,9,14,19,18,17,12,13", buildStringSpiralPrint(list2, List(), n2, m2))


