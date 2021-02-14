import scala.util.Random

/**
 * This problem was asked by Facebook.
 *
 * Given a stream of elements too large to store in memory, pick a random element from the stream with uniform probability.
 * Extra info: https://www.geeksforgeeks.org/select-a-random-number-from-stream-with-o1-space/
 */


  //TODO given the problem description, it's not clear what means uniform probability for me. That means 1/n probability, where n is the visited numbers
//we represent the elements as Ints to simplify
val stream: Array[Int] = Array(1, 2, 3, 4)

//The problem is we can't do a stream.length
var res = 0;    // The resultant random number
var count = 0;  //Count of numbers visited so far in stream

//A method to randomly select a item from stream[0], stream[1], .. stream[i-1]
def selectRandom(x: Int): Int = {
  count = count + 1; // increment count of numbers seen so far

  // If this is the first element from stream, return it
  if (count == 1)
    res = x;
  else
  {
    // Generate a random number from 0 to count - 1
    val r = new Random();
    val i = r.nextInt(count);

    // Replace the prev random number with new number with 1/count probability
    if(i == count - 1)
      res = x;
  }
  res;
}


val n = stream.length
for (i <- 0 until n) {
  System.out.println("Random number from first " + (i + 1) + " numbers is " + selectRandom(stream(i)))
}