package dailyCodingProblem.dailyCodingProblem.medium.Solved.Problem26

import org.junit.Assert.assertEquals

/**
 * This problem was asked by Google.
 *
 * Given a singly linked list and an integer k, remove the kth last element from the list. k is guaranteed to be smaller than the length of the list.
 *
 * The list is very long, so making more than one pass is prohibitively expensive.
 *
 * Do this in constant space and in one pass.
 */
object RemoveElementFromLs extends App {

  val in1 = new LinkedList(1, new LinkedList(2, new LinkedList(3, new LinkedList(4, null))))
  val out1 = new LinkedList(2, new LinkedList(3, new LinkedList(4, null)))
  val out2 = new LinkedList(1, new LinkedList(3, new LinkedList(4, null)))
  val out3 = new LinkedList(1, new LinkedList(2, new LinkedList(4, null)))
  val out4 = new LinkedList(1, new LinkedList(2, new LinkedList(3, null)))

  class LinkedList(val value: Int, val tail: LinkedList) {
    override def toString: String = if (this.tail != null) this.value + " ->" + this.tail.toString else this.value.toString

    def +(that: LinkedList): LinkedList = {
      if (this.tail != null) new LinkedList(this.value, this.tail.+(that))
      else new LinkedList(this.value, that)
    }

    def length(): Int = if (this.tail != null) 1 + this.tail.length() else 1

    override def equals(obj: Any): Boolean = (this.toString == obj.toString)

    def removeKthHelper(k: Int, n: Int, inLength: Int): LinkedList = {
      if (n == 1 && k == inLength - 1) new LinkedList(this.value, null)
      else if (n == 0 && k != inLength) new LinkedList(this.tail.value, this.tail.tail)
      else new LinkedList(this.value, this.tail.removeKthHelper(k, n - 1, inLength))
    }

    def removeKth(k: Int): LinkedList = removeKthHelper(k, k, this.length())
  }
  //
  //  println(in1.toString)
  //  println(in1.length())

  assertEquals(out1, in1.removeKth(0))
  assertEquals(out2, in1.removeKth(1))
  assertEquals(out3, in1.removeKth(2))
  assertEquals(out4, in1.removeKth(3))

}
