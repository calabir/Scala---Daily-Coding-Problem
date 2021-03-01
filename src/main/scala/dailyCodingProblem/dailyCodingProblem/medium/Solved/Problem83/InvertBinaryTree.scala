package dailyCodingProblem.dailyCodingProblem.medium.Solved.Problem83

import org.junit.Assert.assertEquals

/**
 * This problem was asked by Google.
 *
 * Invert a binary tree.
 *
 * For example, given the following tree:
 *
 * a
 * / \
 * b   c
 * / \  /
 * d   e f
 * should become:
 *
 * a
 * /  \
 * c    b
 * \  / \
 * f e  d
 */
object InvertBinaryTree extends App {

  val in1 = new Node("a", new Node("b", new Node("d", null, null), new Leaf("e")), new Node("c", new Leaf("f"), null))
  val out1 = new Node("a", new Node("c", null, new Leaf("f")), new Node("b", new Leaf("e"), new Leaf("d")))

  def invertIt(in: binaryTree): binaryTree = {
    if ((in.left == null || in.left.isLeaf) && (in.right == null || in.right.isLeaf)) new Node(in.content, in.right, in.left)
    else {
      val newLeft = if (in.right != null) invertIt(in.right) else null
      val newRight = if (in.left != null) invertIt(in.left) else null
      new Node(in.content, newLeft, newRight)
    }
  }

  trait binaryTree {
    val left: binaryTree
    val right: binaryTree
    val isLeaf: Boolean
    val content: String
  }

  class Node(val content: String, val left: binaryTree, val right: binaryTree) extends binaryTree {
    val isLeaf = false

    override def toString: String = {
      val leftValue = if (left != null) " left: " + left.toString else ""
      val rightValue = if (right != null) " right: " + right.toString else ""

      this.content + "\n" + leftValue + rightValue
    }
  }

  class Leaf(val content: String) extends binaryTree {
    val left: binaryTree = null
    val right: binaryTree = null
    val isLeaf = true

    override def toString: String = this.content + "\n"

  }

  assertEquals(invertIt(in1).toString, out1.toString)
}
