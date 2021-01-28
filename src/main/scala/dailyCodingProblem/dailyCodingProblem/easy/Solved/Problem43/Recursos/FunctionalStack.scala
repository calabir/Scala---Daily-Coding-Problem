package dailyCodingProblem.dailyCodingProblem.easy.Solved.Problem43.Recursos

/**
 * https://amitdev.github.io/posts/2013-12-31-functional-stack/
 */
class FunctionalStack {
  trait Stack[+T] {
    def isEmpty: Boolean
    def cons[U >: T](t: U): Stack[U]
    def head: T
    def tail: Stack[T]
  }

  sealed abstract class CList[+T] extends Stack[T] {
    def cons[U >: T](t: U): CList[U] = Cons(t, this)
    def ++[U >: T](ys: CList[U]): CList[U]
    def update[U >: T](t: U, i: Int): CList[U]
    def toList: List[T]
  }

  case object Empty extends CList[Nothing] {
    def isEmpty: Boolean = true
    def head: Nothing = throw new NoSuchElementException()
    def tail: Nothing = throw new NoSuchElementException()
    def ++[U](ys: CList[U]): CList[U] = ys
    def update[U](t: U, i: Int) = throw new IndexOutOfBoundsException()
    def toList: Nil.type = Nil
  }

  case class Cons[+T](hd: T, tl: CList[T]) extends CList[T] {
    def isEmpty: Boolean = false
    def head: T = hd
    def tail: CList[T] = tl
    def ++[U >: T](ys: CList[U]): CList[U] = Cons(head, tail ++ ys)
    def update[U >: T](t: U, i: Int): CList[U] = if (i == 0) Cons(t, this) else Cons(head, tail.update(t, i-1))
    def toList: List[T] = head :: tail.toList
  }


  def main (args: Array[String] ): Unit = {
    val stack = Cons(1, Cons(2, Empty)) // head = 1, tail = [2]

  }

}



