import org.junit.Assert.assertEquals

/**
 * This problem was asked by Snapchat.
 *
 * Given an array of time intervals (start, end) for classroom lectures (possibly overlapping), find the minimum number of rooms required.
 *
 * For example, given [(30, 75), (0, 50), (60, 150)], you should return 2.
 * */

case class interval(start: Int, end: Int)

val in1 = Array((30, 75), (0, 50), (60, 150))
val intervalArray = in1.map(x => interval(x._1, x._2))

//         *******************
//case1 *************
//case2          *******
//case3                   *********
//

def collides(in1: interval, in2: interval): Int = {
  if (((in1.start <= in2.start) && (in1.end > in2.start) && (in1.end < in2.end))
    || ((in1.start >= in2.start) && (in1.end <= in2.end))
    || ((in1.start >= in2.start) && (in1.end >= in2.`end`))) 1 else 0
}

def findOverlaps(in: Array[(Int, Int)]): Int = {
  val intervalArray = in.map(x => interval(x._1, x._2))
  (for (i <- intervalArray.indices; j <- i + 1 until intervalArray.length)
    yield collides(intervalArray(i), intervalArray(j))).sum
}

assertEquals(2, findOverlaps(in1))
