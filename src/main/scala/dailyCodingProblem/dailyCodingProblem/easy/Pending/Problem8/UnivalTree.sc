/**
 * This problem was asked by Google.
 *
 * A unival tree (which stands for "universal value") is a tree where all nodes under it have the same value.
 *
 * Given the root to a binary tree, count the number of unival subtrees.
 *
 * For example, the following tree has 5 unival subtrees:
 *          0
 *         / \
 *        1   0
 *       / \
 *      1   0
 *     / \
 *    1   1
 */

class Tree[+T]
case object Empty extends Tree[Nothing]
case class Node[T](val elem: T, val left: Tree[T], val right: Tree[T]) extends Tree[T] {
  override def toString() = elem.toString + '\n' + "left: " + left.toString + '\n' + "right: " + right.toString
}


new Node[Int](1, new Node[Int](2, Empty, Empty), new Node[Int](3, Empty, Empty))



