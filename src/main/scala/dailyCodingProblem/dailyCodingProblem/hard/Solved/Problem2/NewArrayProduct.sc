import org.junit.Assert.assertEquals

import scala.annotation.tailrec

/** This problem was asked by Uber.
 *
 * Given an array of integers, return a new array such that each element at index i of the new array is the product of all the numbers in the original array except the one at i.
 *
 * For example, if our input was [1, 2, 3, 4, 5], the expected output would be [120, 60, 40, 30, 24]. If our input was [3, 2, 1], the expected output would be [2, 3, 6].
 *
 * Follow-up: what if you can't use division?
 */

val array1 = Array(1, 2, 3, 4, 5)
(array1 foldRight 1) ((x, y) => x * y)
val array2 = Array(3, 2, 1)

def computeNewArrayWithDividion(inputList: List[Int]): List[Int] = {
  val total = (inputList foldRight 1) (_ * _)
  inputList map (x => total / x)
}


assertEquals(Array(120, 60, 40, 30, 24).toList, computeNewArrayWithDividion(array1.toList))
assertEquals(Array(2, 3, 6).toList, computeNewArrayWithDividion(array2.toList))


//Now without the division

@tailrec
def indexOf(value: Int, list: List[Int], acc: Int): Int = {
  if (list.head == value) acc
  else indexOf(value, list.tail, acc + 1)
}

@tailrec
def computeNewArray(inputList: List[Int], originalList: List[Int], acc: List[Int]): List[Int] = {
  inputList match {
    case List() => acc
    case _ =>
      val i = indexOf(inputList.head, originalList, 0)
      computeNewArray(inputList.tail,
        originalList,
        acc :+
          (originalList.take(i)
            .appendedAll(
              originalList.drop(i + 1)) foldRight 1) ((x, y) => x * y))
  }
}


assertEquals(Array(120, 60, 40, 30, 24).toList, computeNewArray(array1.toList, array1.toList, List()))
assertEquals(Array(2, 3, 6).toList, computeNewArray(array2.toList, array2.toList, List()))
