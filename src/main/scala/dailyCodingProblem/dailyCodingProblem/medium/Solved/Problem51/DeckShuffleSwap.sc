import scala.annotation.tailrec
import scala.math.random

/**
 * This problem was asked by Facebook.
 *
 * Given a function that generates perfectly random numbers between 1 and k (inclusive), where k is an input,
 * write a function that shuffles a deck of cards represented as an array using only swaps.
 *
 * It should run in O(N) time.
 *
 * Hint: Make sure each one of the 52! permutations of the deck is equally likely.
 */
//Solved using the Fisher–Yates shuffle Algorithm
def rand(k: Int): Int = {
  (random() * k).toInt + 1
}


val range = 1 to 52
val deck = range toArray

deck.foreach(println)

val rangeShort = Array(1, 2, 3, 4, 5)

def swap(in: Array[Int], i_in: Int, j_in: Int): Array[Int] = {
  println("swapping: " + i_in + " with: " + j_in)
  require(i_in <= 52)
  require(i_in > 0)
  require(j_in <= 52)
  require(j_in > 0)

  //Ordering the randoms, this operation is commutative, but implementation is not
  val tmp_i = math.min(i_in, j_in)
  val tmp_j = math.max(i_in, j_in)

  val valueAt_i = in(tmp_i - 1)
  val valueAt_j = in(tmp_j - 1)

  val part1 = in.take(tmp_i - 1) //previous values to i
  val part2 = in.slice(tmp_i, tmp_j - 1) //values between i and j (exclusive)
  val part3 = in.drop(tmp_j) //values after j

  val thisArray = (part1.appended(valueAt_j) ++ part2.appended(valueAt_i) ++ part3)

  List(part1.toList, List(valueAt_j), part2.toList, List(valueAt_i), part3.toList).flatten.toArray
  thisArray
}


def shuffle(in: Array[Int], firstCard: Int): Array[Int] = {
//  println("gonna shuffle the card number: " + firstCard)
//  println("shuffle input: " + in.mkString("Array(", ", ", ")"))
  val card1 = firstCard

  @tailrec
  def card2(firstCardPosition: Int): Int = {
    val thisRand = rand(in.length)
    if (thisRand == firstCardPosition) card2(firstCardPosition)
    else thisRand
  }

  val card2_1 = card2(card1)

  val output = swap(in, card1, card2_1)
//  println("shuffle output: " + output.mkString("Array(", ", ", ")"))
  output
}

@tailrec
def shuffle1(in: Array[Int], outStep: List[Int]): Array[Int] = {
  outStep.length match {
    case 1 => in
    case _ => shuffle1(shuffle(in, outStep.length), outStep.tail)
  }
}

shuffle1(deck, deck.toList)