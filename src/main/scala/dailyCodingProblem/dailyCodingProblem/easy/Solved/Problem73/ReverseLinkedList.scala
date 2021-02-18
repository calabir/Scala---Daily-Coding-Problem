package dailyCodingProblem.dailyCodingProblem.easy.Solved.Problem73

import scala.annotation.tailrec

object ReverseLinkedList extends App {

  

  /** This problem was asked by Google.
   *
   * Given the head of a singly linked list, reverse it in-place.
   * */

  case class linkedList(head: Int, tail: linkedList) {
    def apply(value: Int, next: linkedList): linkedList = linkedList(head, tail)

    override def toString: String = if(this.tail != null) this.head + "->" + this.tail.toString else this.head.toString

    def :+(newElement: Int): linkedList  = {
      if(this.tail == null) linkedList(this.head, linkedList(newElement, null))
      else linkedList(this.head, this.tail :+ newElement)
    }
  }

  def buildLinkedListFromList(in: List[Int]): linkedList = {
    in match {
      case List() => null
      case xs::ys => linkedList(xs, buildLinkedListFromList(ys))
    }
  }

  @tailrec
  def reverse(in: linkedList, acc: List[Int] = List()): linkedList = {
    if(in.tail != null) reverse(in.tail, acc :+ in.head) else buildLinkedListFromList((acc :+ in.head).reverse)
  }


  val ls = linkedList(1, linkedList(2, linkedList(3, linkedList(4, null))))

  println(ls.toString)
  println(reverse((ls :+ 8 :+ 10)).toString)
  println(reverse(ls).toString)

}
