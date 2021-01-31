import org.junit.Assert.assertEquals

import scala.annotation.tailrec

/**
 * This problem was asked by Microsoft.
 *
 * Implement a URL shortener with the following methods:
 *
 * shorten(url), which shortens the url into a six-character alphanumeric string, such as zLg6wl.
 * restore(short), which expands the shortened string into the original url. If no such shortened string exists, return null.
 * Hint: What if we enter the same URL twice?
 */


val url1 = "www.google.es"
val url2 = "www.google.com"
val url3 = "www.google.uk"

var shortMap: Map[String, String] = Map()

val mapping = Map[Int, String](
  1 -> "a",
  2 -> "b",
  3 -> "c",
  4 -> "d",
  5 -> "e",
  6 -> "f",
  7 -> "g",
  8 -> "h",
  9 -> "i",
  10 -> "j",
  11 -> "k",
  12 -> "l",
  13 -> "m",
  14 -> "n",
  15 -> "o",
  16 -> "p",
  17 -> "q",
  18 -> "r",
  19 -> "s",
  20 -> "t",
  21 -> "u",
  22 -> "v",
  23 -> "w",
  24 -> "x",
  25 -> "y",
  26 -> "z",
  27 -> "0",
  28 -> "1",
  29 -> "2",
  30 -> "3",
  31 -> "4",
  32 -> "5",
  33 -> "6",
  34 -> "7",
  35 -> "8",
  36 -> "9")

def randIntToString(): String = {
  mapping((Math.random() * 35).toInt + 1)
}

@tailrec
def generateRandString(): String = {
  val allLetters = (for (_ <- 1 to 6) yield randIntToString()).reduce(_ + _)
  if (shortMap.keys.toList.contains(allLetters)) generateRandString() else allLetters
}

def generateRandomString(url: String): String = {

  if (!shortMap.values.toList.contains(url)) {
    val allLetters = generateRandString()
    shortMap = shortMap + (allLetters -> url)
    allLetters
  } else {
    shortMap.filter(v => v._2 == url).head._1
  }
}



def shorten(url: String): String = {
  generateRandomString(url)
}

def restore(short: String): String = {
  if (shortMap.contains(short)) shortMap(short)
  else throw new Exception("This URL is not yet indexed")
}

shorten(url1)
shorten(url1)
shorten(url2)
shorten(url3)
shorten(url1)

assertEquals("www.google.es", restore(shorten(url1)))
assertEquals("www.google.com", restore(shorten(url2)))
assertEquals("www.google.uk", restore(shorten(url3)))
