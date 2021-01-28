package dailyCodingProblem.dailyCodingProblem.easy.Solved.Problem43

/**
 * Problem43
 *
 * This problem was asked by Amazon.
 *
 * Implement a stack that has the following methods:
 *
 * push(val), which pushes an element onto the stack
 * pop(), which pops off and returns the topmost element of the stack. If there are no elements in the stack, then it should throw an error or return null.
 * max(), which returns the maximum value in the stack currently. If there are no elements in the stack, then it should throw an error or return null.
 * Each method should run in constant time.
 */

trait Stack {
  def isEmpty: Boolean

  def head: Int

  def tail: Stack

  def pop: (Int, Stack)

  //  def push[T](e: T): Stack[T]
  def push(t: Int): Stack
}


class Cons(var head: Int, var tail: Stack) extends Stack {
  def isEmpty = false

  override def toString: String = head + h1(tail)

  def h1(l1: Stack): String = {
    if (!l1.isEmpty) "," + l1.head + h1(l1.tail)
    else ""

  }

  def pop: (Int, Stack) = {
    val output = head

    val newCons = new Cons(tail.head, tail.tail)
    (output, newCons)
  }

  def push(e: Int): Stack = {
    new Cons(e, new Nil)
    new Cons(e, this)
  }

}

class Nil extends Stack {
  def isEmpty: Boolean = true

  def head: Nothing = throw new NoSuchElementException("Nil.head")

  def tail: Nothing = throw new NoSuchElementException("Nil.tail")


  override def toString: String = "Nil"


  def pop: Nothing = throw new NoSuchElementException("Nil.pop")

  def push(e: Int): Cons = {
    new Cons(e, new Nil)
  }

}

object Main extends App {

  val list1 = new Cons(1, new Cons(2, new Cons(3, new Nil)))

  val list2 = new Nil

  println(list2.push(1).push(2).push(3).pop)
}