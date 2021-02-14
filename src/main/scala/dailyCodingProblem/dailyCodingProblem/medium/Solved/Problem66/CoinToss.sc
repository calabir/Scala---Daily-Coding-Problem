import org.junit.Assert.assertEquals

/**
 * This problem was asked by Square.
 *
 * Assume you have access to a function toss_biased() which returns 0 or 1 with a probability that's not 50-50 (but also not 0-100 or 100-0). You do not know the bias of the coin.
 *
 * Write a function to simulate an unbiased coin toss.
 */


def toss_biased(): Int = {
  val n = 4
  //val percentageOf0 = 1/n //1 of 4 times it returns a 0
  val thisRand = (math.random() * n).toInt
  if (thisRand % n == 0) 0 else 1
}

toss_biased()
toss_biased()
toss_biased()
toss_biased()
toss_biased()
toss_biased()
toss_biased()
toss_biased()
toss_biased()
toss_biased()

def unbiased_coin_toss(): Int = {
  val x = toss_biased()
  val y = toss_biased()
  (x, y) match {
    case (1, 0) => 1
    case (0, 1) => 0
    case _ => unbiased_coin_toss()
  }
}
unbiased_coin_toss()
unbiased_coin_toss()
unbiased_coin_toss()
unbiased_coin_toss()
unbiased_coin_toss()
unbiased_coin_toss()
unbiased_coin_toss()
unbiased_coin_toss()
unbiased_coin_toss()
unbiased_coin_toss()

//Test


def findBias(f: => Int): (Int, Int) = {
  val range = 1000000;
  val resultZero = for (_ <- 0 until range) yield {
    val result = f
    if (result == 0) 1 else 0
  }
  val totalZeroCount = resultZero.sum.toDouble
  val totalOneCount = range - totalZeroCount.toDouble
  //  println("totalOneCount: " + totalOneCount)
  //  println("totalZeroCount: " + totalZeroCount)
  val proportionZerosOne = if (totalOneCount < totalZeroCount) (totalZeroCount / totalOneCount) else (totalOneCount / totalZeroCount)
  //  println("proportionZerosOne: " + proportionZerosOne)
  //  println("proportionZerosOne: " + proportionZerosOne.toInt)
  val roundedProportion = if (proportionZerosOne - proportionZerosOne.toInt > 0.5) proportionZerosOne.toInt + 1 else proportionZerosOne.toInt
  //  println("roundedProportion: " + roundedProportion)
  val biasOneOrZero = if (totalOneCount < totalZeroCount) 0 else 1
  (roundedProportion, biasOneOrZero)
}
assertEquals((3, 1), findBias(toss_biased()))
assertEquals(1, findBias(unbiased_coin_toss())._1)