import org.junit.Assert.assertEquals

/** This problem was asked by Google.
 *
 * Given two singly linked lists that intersect at some point, find the intersecting node. The lists are non-cyclical.
 *
 * For example, given A = 3 -> 7 -> 8 -> 10 and B = 99 -> 1 -> 8 -> 10, return the node with value 8.
 *
 * In this example, assume nodes with the same value are the exact same node objects.
 *
 * Do this in O(M + N) time (where M and N are the lengths of the lists) and constant space.
 */


case class linkedList(valueIn: Int, nextIn: linkedList) {
  val value = valueIn
  val next = nextIn

  def apply(value: Int, next: linkedList): linkedList = linkedList(value, next)

  def iterateLinkedList(ls: linkedList): String = {
    val nextValue = if (ls.next != null) "->" + iterateLinkedList(ls.next) else ""
    ls.value.toString + nextValue
  }


  override def toString: String = iterateLinkedList(this)
}

val ls = linkedList(3, linkedList(7, linkedList(8, linkedList(10, null))))
val ls2 = linkedList(99, linkedList(1, linkedList(8, linkedList(10, null))))

println(ls.toString)


def getSeqFromLs(in: linkedList, acc: Seq[Int]): Seq[Int] = {
  in match {
    case last if (in.next == null) => acc :+ in.value
    case _ => getSeqFromLs(in.next, acc :+ in.value)
  }
}

def checkIntersection(in: linkedList, seq: Seq[Int]): Int = {
  in match {
    case intersects if (seq.contains(in.value)) => in.value
    case _ => checkIntersection(in.next, seq)
  }
}

def intersects(in1: linkedList, in2: linkedList): Int = {
  val seq = getSeqFromLs(in1, Seq())
  checkIntersection(in2, seq)
}

assertEquals(8, intersects(ls, ls2))
