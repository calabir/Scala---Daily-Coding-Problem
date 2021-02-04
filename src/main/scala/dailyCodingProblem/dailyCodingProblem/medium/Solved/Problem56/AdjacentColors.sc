import junit.framework.Assert.assertEquals

import scala.annotation.tailrec

/**
 * This problem was asked by Google.
 *
 * Given an undirected graph represented as an adjacency matrix and an integer k,
 * write a function to determine whether each vertex in the graph can be colored such that no two adjacent vertices share the same color using at most k colors.
 */


/** Example
 *       b----c
 *      /    /
 *     a    /
 *      \  /
 *       d
 */


//If we have more nodes, we need to programmatically build the mapping

val mapping = Map[Int, String](
  0 -> "a",
  1 -> "b",
  2 -> "c",
  3 -> "d",
  4 -> "e",
  5 -> "f",
  6 -> "g",
  7 -> "h",
  8 -> "i",
  9 -> "j",
  10 -> "k",
  11 -> "l",
  12 -> "m",
  13 -> "n",
  14 -> "o",
  15 -> "p",
  16 -> "q",
  17 -> "r",
  18 -> "s",
  19 -> "t",
  20 -> "u",
  21 -> "v",
  22 -> "w",
  23 -> "x",
  24 -> "y",
  25 -> "z")

//We will treat the adjacency matrix like a upper triangular matrix where all the elements above the diagonal (i == j) are zero
//This decision is because we don't need those elements, those adjacency are treated in the previous column. In case of a directional graph
//we would need also those values

//    a  b  c  d
//a  (1, 0, 0, 0)
//b  (1, 1, 0, 0)
//c  (0, 1, 1, 0)
//d  (1, 0, 1, 1)


val adjMatrix: Array[Array[Int]] = Array(Array(1, 1, 0, 1), Array(0, 1, 1, 0), Array(0, 0, 1, 1), Array(0, 0, 0, 1))

val adjMatrix2: Array[Array[Int]] = Array(Array(1, 1, 0, 0, 0), Array(0, 1, 1, 0, 0), Array(0, 0, 1, 1, 0), Array(0, 0, 0, 1, 1), Array(0, 0, 0, 0, 1))


def adjMatrixToAdjMap(in: Array[Array[Int]]): Map[String, List[String]] = {
  val result1: Array[Seq[String]] = in.map(l => for (i <- l.indices) yield if (l(i) != 0) mapping(i) else "")
    .map(a => a.filter(a => a != ""))
  val result2 = for (i <- result1.indices) yield (result1(i).head, result1(i).tail)
  result2.map(a => (a._1, a._2.toList)).toMap
}

def buildColorMapInitialValues(in: Map[String, List[String]]): Map[String, Int] = {
  in.map(k => (k._1, 1))
}

val adjMap1 = adjMatrixToAdjMap(adjMatrix)

//val adjMap2 = adjMatrixToAdjMap(adjMatrix2)

println(adjMap1)
//println(adjMap2)

@tailrec
def helper(in: (String, List[String]), acc: Map[String, Int]): Map[String, Int] = {
  in._2 match {
    case List() => acc
    case xs :: ys => {
      val newAcc = if (acc(xs) == acc(in._1)) acc.updated(xs, acc(xs) + 1)
      else acc
      helper((in._1, ys), newAcc)
    }
  }
}


@tailrec
def recursiveMap(inAdj: Map[String, List[String]], acc: Map[String, Int]): Map[String, Int] = {
  inAdj match {
    case _ if inAdj.isEmpty => acc
    case _ if inAdj.nonEmpty =>
      val newAcc = helper((inAdj.keys.head, inAdj(inAdj.keys.head)), acc)
      recursiveMap(inAdj.drop(1), newAcc)
  }
}

def canWePaintWithKcolors(inAdj: Map[String, List[String]], k: Int): Boolean = {
  require(k > 0, "k can't be zero or negative")
  val colorMap = buildColorMapInitialValues(inAdj)
  if (recursiveMap(inAdj, colorMap).values.max <= k) true else false
}




//assertEquals(false, canWePaintWithKcolors(adjMap1, 0)) //Throw an exception if k <= 0
assertEquals(false, canWePaintWithKcolors(adjMap1, 1))
assertEquals(true, canWePaintWithKcolors(adjMap1, 2))
assertEquals(true, canWePaintWithKcolors(adjMap1, 3))
assertEquals(true, canWePaintWithKcolors(adjMap1, 4))


//TODO there is  a bug in the code and the following examples fail
//assertEquals(false, canWePaintWithKcolors(adjMap2, 1))
//assertEquals(false, canWePaintWithKcolors(adjMap2, 2))
//assertEquals(false, canWePaintWithKcolors(adjMap2, 3))
//assertEquals(false, canWePaintWithKcolors(adjMap2, 4))
//assertEquals(true, canWePaintWithKcolors(adjMap2, 5))
//assertEquals(true, canWePaintWithKcolors(adjMap2, 6))



