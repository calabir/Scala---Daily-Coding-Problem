
import org.junit.Assert.assertEquals

import scala.annotation.tailrec

/**
 * This problem was asked by Google.
 *
 * In a directed graph, each node is assigned an uppercase letter. We define a path's value as the number of most frequently-occurring letter along that path. For example, if a path in the graph goes through "ABACA", the value of the path is 3, since there are 3 occurrences of 'A' on the path.
 *
 * Given a graph with n nodes and m directed edges, return the largest value path of the graph. If the largest value is infinite, then return null.
 *
 * The graph is represented with a string and an edge list. The i-th character represents the uppercase letter of the i-th node. Each tuple in the edge list (i, j) means there is a directed edge from the i-th node to the j-th node. Self-edges are possible, as well as multi-edges.
 *
 * For example, the following input graph:
 *
 * ABACA
 * [(0, 1),
 * (0, 2),
 * (2, 3),
 * (3, 4)]
 * Would have maximum value 3 using the path of vertices [0, 2, 3, 4], (A, A, C, A).
 *
 * The following input graph:
 *
 * A
 * [(0, 0)]
 * Should return null, since we have an infinite loop.
 * */



@tailrec
def findNodeInList(in: (Int, Int), edges: List[(Int, Int)], n: Int, visitedNode: List[String], nodesMapping: Map[Int,String]): List[String] = {
  if(in._1 == in._2) throw new Exception("There is a loop")
  else{
    val actualNode = nodesMapping.get(in._1)
    edges match {
      case List() =>
        visitedNode :++ nodesMapping.get(in._1) :++ nodesMapping.get(in._2)
      case xs::ys =>
        if(in._2 == xs._1) {
          findNodeInList(xs, ys, n+1, visitedNode :++ actualNode, nodesMapping)
        }else findNodeInList(in, ys, n, visitedNode, nodesMapping)
    }
  }
}


@tailrec
def iterateAllEdges(in: List[(Int, Int)], edges: List[(Int, Int)], acc: List[List[String]], nodesMapping: Map[Int,String]): List[List[String]] = {
  in match {
    case List() => acc
    case xs::ys => {
      val newAcc = acc :+ findNodeInList(xs, edges, 1, List(), nodesMapping)
      iterateAllEdges(ys, edges, newAcc, nodesMapping)}
  }
}


def nodesToMap(in: String): Map[Int,String] = {
  in.toArray.zipWithIndex.map(x => (x._2, x._1.toString)).toMap
}

def nWays(nodes: String, edges: List[(Int, Int)]): Int ={
  val nodesMapping = nodesToMap(nodes)
  val listOfResults = iterateAllEdges(edges, edges, List(), nodesMapping)
  val reduced = listOfResults.map(x => x.map(y => (y,1)).groupBy(_._1).collect{
    case e => e._1 -> e._2.map(_._2).sum
  }.toList)
  reduced.flatMap(x => x.map(y => y._2)).max
}

val in1 = "ABACA"
val edges1 = List((0,1),(0, 2),(2, 3),(3, 4))
val res1: Int = 3

assertEquals(3, nWays(in1, edges1))

val in2 = "A"
val edges2 = List((0,0))

try {
  nWays(in2, edges2)
} catch {
  case expectedException: Exception => assert(expectedException.getMessage == "There is a loop")
  case _ => println("This wasn't expected")
}