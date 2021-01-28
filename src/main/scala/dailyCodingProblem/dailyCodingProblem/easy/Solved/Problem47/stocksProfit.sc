import org.junit.Assert.assertEquals

/**
This problem was asked by Facebook.

Given a array of numbers representing the stock prices of a company in chronological order, write a function that calculates the maximum profit you could have made from buying and selling that stock once. You must buy before you can sell it.

For example, given [9, 11, 8, 5, 7, 10], you should return 5, since you could buy the stock at 5 dollars and sell it at 10 dollars.
*/


val stockPrices = List(9, 11, 8, 5, 7, 10)


def maxBetweenPairs(buy: Int, sell: Int): Int = {
  if(buy < sell) sell - buy
  else 0
}

def computeBenefit(prices: List[Int]): Int = {
  val allPairs = for{
  buy <- 0 to prices.length
  sell <- prices.drop(buy+1)
  } yield maxBetweenPairs(prices.drop(buy).head,sell)

  allPairs.distinct.max
}


assertEquals(5,computeBenefit(stockPrices))

def computeBenefitOpt(prices: List[Int], max: Int): Int = {

  prices match {
    case _ ::Nil => max
    case xs::ys =>
      val maxNow = if(ys.max - xs > max) ys.max - xs else max
      computeBenefitOpt(ys, maxNow)
  }
}

computeBenefitOpt(stockPrices, 0)
