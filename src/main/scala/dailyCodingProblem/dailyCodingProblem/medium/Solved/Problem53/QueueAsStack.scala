package dailyCodingProblem.dailyCodingProblem.medium.Solved.Problem53

/** This problem was asked by Apple.
 *
 * Implement a queue using two stacks. Recall that a queue is a FIFO (first-in, first-out) data structure with the following methods: enqueue, which inserts an element into the queue, and dequeue, which removes it.
 * */


//In Scala Stacks are deprecated and immutable lists should be used instead


class QueueAsStack(content: List[Int]) {
  def enqueue(element: Int): QueueAsStack = {
    new QueueAsStack(this.content :+ element)
  }

  def dequeue(): QueueAsStack = {
    new QueueAsStack(this.content.take(this.content.length - 1))
  }

  def apply(content: List[Int]): QueueAsStack = new QueueAsStack(content)

  override def toString: String = this.content.mkString(",")
}

