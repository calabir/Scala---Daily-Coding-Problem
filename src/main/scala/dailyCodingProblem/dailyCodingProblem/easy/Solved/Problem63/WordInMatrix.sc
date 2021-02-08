import org.junit.Assert.assertEquals

/**
 * This problem was asked by Microsoft.
 *
 * Given a 2D matrix of characters and a target word, write a function that returns whether the word can be found in the matrix by going left-to-right, or up-to-down.
 *
 * For example, given the following matrix:
 *
 * [['F', 'A', 'C', 'I'],
 * ['O', 'B', 'Q', 'P'],
 * ['A', 'N', 'O', 'B'],
 * ['M', 'A', 'S', 'S']]
 * and the target word 'FOAM', you should return true, since it's the leftmost column. Similarly, given the target word 'MASS', you should return true, since it's the last row.
 */


val input1 = List(
  List("F", "A", "C", "I"),
  List("O", "B", "Q", "P"),
  List("A", "N", "O", "B"),
  List("M", "A", "S", "S"))

val input2 = Array(
  Array("F", "A", "C", "I"),
  Array("O", "B", "Q", "P"),
  Array("A", "N", "O", "B"),
  Array("M", "A", "S", "S"))



def verticalWords(in: Array[Array[String]], i: Int, j: Int, acc: String): String = {
  val newAcc = acc + in(j)(i)
  if (j < 3) verticalWords(in, i, j + 1, newAcc)
  else if (i < 3) verticalWords(in, i + 1, 0, newAcc + "#")
  else newAcc
}


verticalWords(input2, 0, 0, "").split("#").toList

def wordInMatrix(target: String): Boolean = {
  val acc = input1.map(l => l.foldRight("")(_ + _)) :++ verticalWords(input2, 0, 0, "").split("#").toList
  if (acc.contains(target)) true else false
}

val initialAcc = input1.map(l => l.foldRight("")(_ + _)) :++ verticalWords(input2, 0, 0, "").split("#").toList

assertEquals(true, wordInMatrix("FOAM"))
assertEquals(true, wordInMatrix("ANOB"))
assertEquals(false, wordInMatrix("qwerty"))

