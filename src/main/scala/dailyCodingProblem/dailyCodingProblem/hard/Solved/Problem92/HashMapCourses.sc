import org.junit.Assert.assertEquals

import scala.annotation.tailrec
import scala.collection.immutable.HashMap

/**
 * This problem was asked by Airbnb.
 *
 * We're given a hashmap associating each courseId key with a list of courseIds values, which represents that the prerequisites of courseId are courseIds. Return a sorted ordering of courses such that we can finish all courses.
 *
 * Return null if there is no such ordering.
 *
 * For example, given {'CSC300': ['CSC100', 'CSC200'], 'CSC200': ['CSC100'], 'CSC100': []}, should return ['CSC100', 'CSC200', 'CSCS300'].
 * */


val hm1 = HashMap("CSC300" -> List("CSC100", "CSC200"),
  "CSC200" -> List("CSC100"),
  "CSC100" -> List())


@tailrec
def coursesList(in: HashMap[String, List[String]], acc: List[String]): List[String] = {
  val startingCourses = in.toList.filter(x => acc.contains(x._2) || acc == x._2).map(_._1)
  if (startingCourses.isEmpty && acc.isEmpty) throw new Exception("No such ordering exist")
  val newAcc = acc :++ startingCourses
  if (newAcc != acc) {
    coursesList(in, newAcc)
  } else acc
}

assertEquals(List("CSC100", "CSC200", "CSC300"), coursesList(hm1, List()))