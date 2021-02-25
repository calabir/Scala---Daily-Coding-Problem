package dailyCodingProblem.dailyCodingProblem.medium.Solved.Problem78

import org.junit.Assert.assertEquals

import scala.annotation.tailrec

/**
 * This problem was asked by Google.
 *
 * Given k sorted singly linked lists, write a function to merge all the lists into one sorted singly linked list.
 * */

object MergedLinkedList extends App {

  val ls1 = new linkedList(1, new linkedList(2, new linkedList(3, null)))
  val ls2 = new linkedList(3, new linkedList(4, new linkedList(5, null)))
  val ls3 = new linkedList(6, new linkedList(7, new linkedList(8, null)))

  val ls4 = new linkedList(0, new linkedList(2, new linkedList(4, null)))
  val ls5 = new linkedList(1, new linkedList(3, new linkedList(5, null)))

  val sq1: Seq[Int] = Seq(1, 2, 3, 4) :+ 5


  val listOfLs1 = List(ls2, ls3, ls1)
  val listOfLs2 = List(ls4, ls5)

  @tailrec
  def mapLists(in: List[linkedList], acc: Seq[Int]): Seq[Int] = {
    in match {
      case List() => acc
      case xs :: ys =>
        mapLists(ys, xs.mapList(acc))
    }
  }

  def seqToLs(in: Seq[Int]): linkedList = {
    in match {
      case Seq() => null
      case xs :: ys =>
        new linkedList(xs, seqToLs(ys))
    }
  }

  def mergeListOfLinkedList(in: List[linkedList]): linkedList = {
    seqToLs(mapLists(in, Seq()).sorted(Ordering.Int))
  }

  assertEquals("1,2,3,4,5,6,7,8", mergeListOfLinkedList(listOfLs1).toString)
  assertEquals("0,1,2,3,4,5", mergeListOfLinkedList(listOfLs2).toString)


  class linkedList(head: Int, tail: linkedList) {
    override def toString: String = if (this.tail != null) head + "," + tail.toString else head.toString

    def modifyLastOne(that: linkedList): linkedList = {
      if (this.tail != null) new linkedList(this.head, this.tail.modifyLastOne(that))
      else new linkedList(this.head, that)
    }

    def +(that: linkedList): linkedList = this.modifyLastOne(that)

    def mapList(inSeq: Seq[Int]): Seq[Int] = {
      val newSeq = if (!inSeq.contains(this.head)) inSeq.appended(this.head) else inSeq
      if (this.tail != null) this.tail.mapList(newSeq)
      else newSeq
    }
  }
}
