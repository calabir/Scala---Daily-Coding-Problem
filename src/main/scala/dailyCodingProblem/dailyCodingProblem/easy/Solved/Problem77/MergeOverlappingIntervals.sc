import org.junit.Assert.assertEquals

/**
 * This problem was asked by Snapchat.
 *
 * Given a list of possibly overlapping intervals, return a new list of intervals where all overlapping intervals have been merged.
 *
 * The input list is not necessarily ordered in any way.
 *
 * For example, given [(1, 3), (5, 8), (4, 10), (20, 25)], you should return [(1, 3), (4, 10), (20, 25)].
 * */


case class interval(start: Int, end: Int)


// if two intervals collide, keep the bigger one
val in1 = Array((1, 3), (5, 8), (4, 10), (20, 25))
val inIntervals1 = in1.map(x => interval(x._1, x._2))

def overlappedIntervals(int1: interval, int2: interval): List[interval] = {
  //int1 is a sub-interval of int2, so we keep int2
  if (int1.start >= int2.start && int1.end <= int2.end)
    List(int1)
  //int2 is a sub-interval of int1, so we keep int1
  else if (int2.start >= int1.start && int2.end <= int1.end)
    List(int2)
  else {
    //no overlap
    List()
  }

}

def mergedIntervals(in: Array[interval]): Array[(Int, Int)] = {
  val overlappingIntervals = for (i <- 0 until in.length - 1; j <- i + 1 until in.length) yield {
    overlappedIntervals(in(i), in(j))
  }
  val uniqueOverlappingIntervals = overlappingIntervals.flatten.toSet.toArray
  val thisOut = in.diff(uniqueOverlappingIntervals).map(x => (x.start, x.`end`))
  thisOut
}

assertEquals(Array((1, 3), (4, 10), (20, 25)).mkString(","), mergedIntervals(inIntervals1).mkString(","))
