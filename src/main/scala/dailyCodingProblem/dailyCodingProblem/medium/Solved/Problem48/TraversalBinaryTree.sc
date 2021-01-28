import scala.annotation.tailrec

/**
This problem was asked by Google.

Given pre-order and in-order traversals of a binary tree, write a function to reconstruct the tree.

For example, given the following preorder traversal:

[a, b, d, e, c, f, g]

And the following inorder traversal:

[d, b, e, a, f, c, g]

You should return the following tree:

    a
   / \
  b   c
 / \ / \
d  e f  g

*/

/**
 * In a Preorder sequence, leftmost element is the root of the tree. So we know ‘A’ is root for given sequences.
 * By searching ‘A’ in Inorder sequence, we can find out all elements on left side of ‘A’ are in left subtree and elements on right are in right subtree
 */


val preorder0 = List("a","b","d","e","c","f","g")
val inorder0 = List("d", "b", "e", "a", "f", "c", "g")

val fillLength = 15

def treeFun(preord0: List[String], inord: List[String], depth: Int, acc: List[(String, Int)]): List[(String, Int)] = {
  val preord: List[String] = preord0.flatMap(s => inord.map(s1 => if (s == s1) s else "")).filter(s => s != "")

  @tailrec
  def searchCharInList(list: List[String], valueToFind: String, acc: Int): Int = {
    list match {
      case List() => 0
      case xs::ys => if(xs == valueToFind) acc else searchCharInList(ys, valueToFind, acc+1)
    }
  }
  val preHead = preord.head

  val positionOf = searchCharInList(inord, preHead, 0)

  val left = inord.splitAt(positionOf)._1
  val right = inord.splitAt(positionOf+1)._2


  //TODO Now I can go through the tree, but I'd like to print the tree in a good way in the console

  println(Vector.fill(20)("*").updated((20/2)-depth, preHead))
  println("preorder.head: " + preHead)
  println("depth: " + depth)

  val output = (preHead, depth)
  if(preord.tail.length > 1 && left.nonEmpty) treeFun(preord.tail, left, depth+1, acc :+ output)
  if(preord.tail.length > 1 && right.nonEmpty) treeFun(preord.tail, right, depth+1, acc :+ output)
  acc
}

println(treeFun(preorder0, inorder0, 1, List(("",0))))















