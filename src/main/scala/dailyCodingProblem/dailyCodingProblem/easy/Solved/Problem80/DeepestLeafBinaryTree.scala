package dailyCodingProblem.dailyCodingProblem.easy.Solved.Problem80

import org.junit.Assert.assertEquals

object DeepestLeafBinaryTree extends App {

  /**
   * This problem was asked by Google.
   *
   * Given the root of a binary tree, return a deepest node. For example, in the following tree, return d.
   *
   *     a
   *    / \
   *   b   c
   *  /
   * d
   *
   */
    trait binaryTree {
      val value: String
      val left: binaryTree
      val right: binaryTree
    }

    class node(val value: String, val left: binaryTree, val right: binaryTree) extends binaryTree

    class leaf(val value: String) extends binaryTree {
      val left: binaryTree = null
      val right: binaryTree = null
    }


    def goTroughTree(in: binaryTree, currentDepth: Int): List[(String, Int)] = {
      val left = if(in.left != null) goTroughTree(in.left, currentDepth + 1) else  List()

      val right = if(in.right != null) goTroughTree(in.right, currentDepth + 1) else  List()

      List((in.value, currentDepth)) :++ left :++ right
    }

    def getMaxDepthNode(in: binaryTree): String = {
      goTroughTree(in,0).maxBy(_._2)._1
    }

    val in1 = new node("a", new node("b", new leaf("d"), null), new leaf("c"))

    assertEquals("d" ,getMaxDepthNode(in1))
  }
