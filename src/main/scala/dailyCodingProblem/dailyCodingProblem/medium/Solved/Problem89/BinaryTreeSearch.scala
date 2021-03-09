package dailyCodingProblem.dailyCodingProblem.medium.Solved.Problem89

import org.junit.Assert.assertEquals


/**
 * This problem was asked by LinkedIn.
 *
 * Determine whether a tree is a valid binary search tree.
 *
 * A binary search tree is a tree with two children, left and right, and satisfies the constraint that 
 * the key in the left child must be less than or equal to the root and the key in the right child must be greater than or equal to the root.
 */
object BinaryTreeSearch extends App {

  val in1 = binaryTree(5, binaryTree(8, binaryTree(3, null, null), binaryTree(5, null, null)), binaryTree(6, binaryTree(4, null, null), binaryTree(7, null, null)))
  val in2 = binaryTree(8, binaryTree(3, binaryTree(1, null, null), binaryTree(6, binaryTree(4, null, null), binaryTree(7, null, null))), binaryTree(10, null, binaryTree(14, binaryTree(13, null, null), null)))

  def isValid(in: binaryTree): (Int, Boolean) = {
    in match {
      case binaryTree(_, null, null) => (in.value, true)
      case binaryTree(_, _, null) =>
        if (in.value <= isValid(in.left)._1) (in.value, true)
        else (in.value, false)
      case binaryTree(_, null, _) =>
        if (in.value >= isValid(in.right)._1) (in.value, true)
        else (in.value, false)
      case binaryTree(_, _, _) =>
        if (in.value >= isValid(in.left)._1 && in.value <= isValid(in.right)._1) (in.value, true)
        else (in.value, false)
    }
  }

  case class binaryTree(value: Int, left: binaryTree, right: binaryTree)

  assertEquals(false, isValid(in1)._2)
  assertEquals(true, isValid(in2)._2)
}
