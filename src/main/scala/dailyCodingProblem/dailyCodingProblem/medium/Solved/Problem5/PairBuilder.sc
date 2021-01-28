import junit.framework.Assert.assertEquals

/**
This problem was asked by Jane Street.

cons(a, b) constructs a pair, and car(pair) and cdr(pair) returns the first and last element of that pair. For example, car(cons(3, 4)) returns 3, and cdr(cons(3, 4)) returns 4.

Given this implementation of cons:

def cons(a, b):
    def pair(f):
        return f(a, b)
    return pair
Implement car and cdr.
*/


def cons[T](a: T, b: T) = {

  def pair() = {
    (a, b)
  }
  pair()
}

cons("a", "b")

val pair1: (String, String) = ("a", "b")

def car[T](cons1: (T, T)): T = {
  cons1._1
}
def cdr[T](cons1: (T, T)): T = {
  cons1._2
}


assertEquals(3, car(cons(3, 4)))
assertEquals(4, cdr(cons(3, 4)))
