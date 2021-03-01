import org.junit.Assert.assertEquals

/**
 * This problem was asked by Facebook.
 *
 * Given three 32-bit integers x, y, and b, return x if b is 1 and y if b is 0, using only mathematical or bit operations. You can assume b can only be 1 or 0.
 * */


def returnIt(x: Int, y: Int, b: Int): Int = x*b+y*(1-b)


assertEquals(5, returnIt(5, 8, 1))
assertEquals(8, returnIt(5, 8, 0))