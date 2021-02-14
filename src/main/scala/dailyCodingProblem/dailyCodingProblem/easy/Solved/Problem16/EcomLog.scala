package dailyCodingProblem.dailyCodingProblem.easy.Solved.Problem16

import org.junit.Assert.assertEquals

object EcomLog extends App {

  val order = new Orders(List(1))
  val manyOrders = order.record(2).record(3).record(4).record(5).record(6).record(7)

  /**
   * This problem was asked by Twitter.
   *
   * You run an e-commerce website and want to record the last N order ids in a log. Implement a data structure to accomplish this, with the following API:
   *
   * record(order_id): adds the order_id to the log
   * get_last(i): gets the ith last element from the log. i is guaranteed to be smaller than or equal to N.
   * You should be as efficient with time and space as possible.
   */


  /*
  Log example:
  order1
  order2
  order3
  order4
  order5
  order6
  order7
  --------------
  record(8)
  --------------
  Log now:
  order1
  order2
  order3
  order4
  order5
  order6
  order7
  order8 -- N
  --------------
  get_last(2) -> order6
  --------------

   */

  class Orders(content: List[Int]) {
    def record(order_id: Int): Orders = {
      new Orders(this.content :+ order_id)
    }

    def get_last(i: Int): Int = {
      this.content.dropRight(i).last
    }

    def apply(content: List[Int]): Orders = new Orders((content))


    override def toString: String = this.content.mkString("\n")

  }

  println(manyOrders)
  assertEquals(5, manyOrders.get_last(2))


}
