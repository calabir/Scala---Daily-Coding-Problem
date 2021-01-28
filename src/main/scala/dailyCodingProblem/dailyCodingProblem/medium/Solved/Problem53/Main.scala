package dailyCodingProblem.dailyCodingProblem.medium.Solved.Problem53

import org.junit.Assert.assertEquals

object Main extends App {

  val list = List()
  val l1 = new QueueAsStack(list)

  assertEquals(l1.enqueue(5)
    .enqueue(3)
    .enqueue(8)
    .enqueue(1)
    .dequeue().toString, new QueueAsStack(List(5,3,8)).toString)

}
