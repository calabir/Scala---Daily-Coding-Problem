import org.junit.Assert.assertEquals

/**
 * This question was asked by ContextLogic.
 *
 * Implement division of two positive integers without using the division, multiplication, or modulus operators. Return the quotient as an integer, ignoring the remainder.
 * */


val in1 = 8

def divideIt(dividend: Int, divisor: Int): Int = {
  if(dividend > 0) 1 + divideIt(dividend - divisor, divisor)
  else 0
}


assertEquals(2, divideIt(8,4))
assertEquals(0, divideIt(0,4))
//assertEquals(0, divideIt(8,0))