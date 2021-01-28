import junit.framework.Assert.assertEquals

/**
 * This problem was asked by Facebook.
 *
 * Given the mapping a = 1, b = 2, ... z = 26, and an encoded message, count the number of ways it can be decoded.
 *
 * For example, the message '111' would give 3, since it could be decoded as 'aaa', 'ka', and 'ak'.
 *
 * You can assume that the messages are decodable. For example, '001' is not allowed.
 */


val mapping = Map[String, String]("1" -> "a",
  "2" -> "b",
  "3" -> "c",
  "4" -> "d",
  "5" -> "e",
  "6" -> "f",
  "7" -> "g",
  "8" -> "h",
  "9" -> "i",
  "10" -> "j",
  "11" -> "k",
  "12" -> "l",
  "13" -> "m",
  "14" -> "n",
  "15" -> "o",
  "16" -> "p",
  "17" -> "q",
  "18" -> "r",
  "19" -> "s",
  "20" -> "t",
  "21" -> "u",
  "22" -> "v",
  "23" -> "w",
  "24" -> "x",
  "25" -> "y",
  "26" -> "z")


def decodingWays(message: String): Int = {
  if (message.isEmpty || (message.length == 1 && mapping.contains(message.head.toString))) {
    1
  }
  else {
    val twoDigits = if (message.length >= 2 && mapping.contains(message.take(2))) 1 else 0
    decodingWays(message.tail) + twoDigits
  }
}


decodingWays("111")

assertEquals(3, decodingWays("111"))


def decodingWays2(message: String, acc: List[String]): List[String] = {
  if (message.isEmpty || !mapping.contains(message.head.toString))
    return acc
  else if(message.length == 1 && mapping.contains(message.head.toString)) {
    acc :+ mapping(message.head.toString)
  }

  val twoDigits = if (message.length >= 2 && mapping.contains(message.take(2))) decodingWays2(message.drop(2), acc :+ mapping(message.take(2))) else acc
  decodingWays2(message.tail, acc :+ mapping(message.head.toString)) :++ twoDigits

}

decodingWays2("111", List())


def test(input: String, acc: List[String]): List[String] = {
  input match {
    case "" => acc
    case _ => test(input.tail, acc :+ (mapping(input.head.toString)))// :++ test(input.tail, acc.appended(mapping(input.take(2))))
  }
}

val input = "111"
val acc = List()

val take2 = mapping(input.take(2))
acc :+ take2


//TODO how to show all the combinations now?