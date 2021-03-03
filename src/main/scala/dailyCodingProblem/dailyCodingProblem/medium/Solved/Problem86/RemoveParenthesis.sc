import org.junit.Assert.assertEquals

import scala.annotation.tailrec

/**
 * This problem was asked by Google.
 *
 * Given a string of parentheses, write a function to compute the minimum number of parentheses to be removed to make the string valid (i.e. each open parenthesis is eventually closed).
 *
 * For example, given the string "()())()", you should return 1. Given the string ")(", you should return 2, since we must remove all of them.
 * */


val in1 = "()())()"
val in2 = ")("
val in3 = "))))"
val in4 = "(((("
val in5 = "))(()(("

@tailrec
def nWays(in: String, accOpened: Int, accClosed: Int): Int = {
  if(in.isEmpty) accOpened + accClosed
  else if(in.head == ')')
    if(accOpened != 0) nWays(in.tail, accOpened - 1, accClosed)
    else nWays(in.tail, accOpened, accClosed + 1)
  else nWays(in.tail, accOpened + 1, accClosed)
}

assertEquals(1, nWays(in1, 0, 0))
assertEquals(2, nWays(in2, 0, 0))
assertEquals(4, nWays(in3, 0, 0))
assertEquals(4, nWays(in4, 0, 0))
assertEquals(5, nWays(in5, 0, 0))
