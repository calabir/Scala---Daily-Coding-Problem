import scala.annotation.tailrec

/**
 * This problem was asked by Google.
 *
 * Implement integer exponentiation. That is, implement the pow1(x, y) function, where x and y are integers and returns x^y.
 *
 * Do this faster than the naive method of repeated multiplication.
 *
 * For example, pow1(2, 10) should return 1024.
 *
 */



//2^10 = 2*2*2*2*2*2*2*2*2*2 = 4*4*4*4*4 = 16*16*4 = 256*4 = 1024

/**
 * We generate the list of elements that conforms b^e
 * I.e. 2^5 = List(2,2,2,2,2)
 *
 * @param b   base of the element
 * @param e   exponent of the element
 * @param acc Intermediate accumulator
 * @return List of integers that conforms b^e
 */
@tailrec
def expoList(b: Int, e: Int, acc: List[Int]): List[Int] = {
  if (e == 0) acc
  else expoList(b, e - 1, acc :+ b)
}

/**
 * Function to reduce a list of integers into another one.
 * I.e inList = List(2,2,2,2,2,2) will be transformed to List(4,4,4)
 *
 * @param inList Input list to be reduced
 * @param acc    Intermediate accumulator
 * @return The reduced list
 */
def reduceList(inList: List[Int], acc: List[Int]): List[Int] = {
  inList match {
    case List() => acc
    case _ if inList.tail.isEmpty => acc :+ inList.head
    case xs :: ys => if (xs == ys.head) reduceList(ys.tail, acc :+ xs * xs) else reduceList(List(), acc :+ ys.head)
  }
}

/**
 * Function to reduce many times a list until it's not possible to reduce more
 * I.e. List(2,2,2,2,2,2) = List(4,4,4) = List (16,4)
 *
 * @param inList Input list to be reduced
 * @return Reduced list
 */
def reduceManyTimes(inList: List[Int]): List[Int] = {
  val reducedList = reduceList(inList, List())
  reducedList match {
    case _ if reducedList.tail.isEmpty => reducedList
    case xs :: ys if xs != ys.head => reducedList
    case _ => reduceManyTimes(reducedList)
  }
}

/**
 * Computes the exponentiation without using the recursive multiplication
 *
 * @param x base of the element
 * @param y exponent of the element
 * @return result of x^y
 * */

def pow(x: Int, y: Int): Int = {
  val reducedList = reduceManyTimes(expoList(x, y, List()))
  reducedList match {
    case oneElement if reducedList.length == 1 => reducedList.head
    case xs :: ys => xs * ys.head
  }

}

pow(2, 10)

//Another way

def pow1(x: Int, y: Int): Int = {
  y match {
    case 0 => 1
    case -1 => 1 / x
    case 1 => x
    case _ if y % 2 == 0 => pow1(x, y / 2) * pow1(x, y / 2)
    case _ => x * pow1(x, (y - 1) / 2) * pow1(x, (y - 1) / 2) //If y is even, we pull out one x and continue as usual
  }
}

pow1(2, 10)