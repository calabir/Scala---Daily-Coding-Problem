package dailyCodingProblem.dailyCodingProblem.medium.Solved.Problem83

import org.junit.Assert.assertEquals

/**
 * This problem was asked by Google.
 *
 * Invert a binary tree (balanced)
 *
 * For example, given the following tree:
 *
 *     a
 *    / \
 *   b   c
 * / \  / \
 * d e  f g
 * should become:
 *
 *     a
 *   /  \
 *  c    b
 * / \  / \
 * g f e  d
 */
object InvertBinaryTree2 extends App {

  val in1 = binaryTree("a", binaryTree("b", binaryTree("d", null, null),
    binaryTree("e", null, null)), binaryTree("c", binaryTree("f", null, null),
    binaryTree("g", null, null)))
  val out1 = binaryTree("a", binaryTree("c", binaryTree("g", null, null), binaryTree("f", null, null)),
    binaryTree("b", binaryTree("e", null, null), binaryTree("d", null, null)))

  def invertIt(in: binaryTree): binaryTree = {
    if (in == null) null
    else {
      val newLeft = invertIt(in.right)
      val newRight = invertIt(in.left)
      binaryTree(in.content, newLeft, newRight)
    }
  }

  case class binaryTree(content: String, left: binaryTree, right: binaryTree)

  assertEquals(invertIt(in1).toString, out1.toString)
}
