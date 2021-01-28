/**
 * This problem was asked by Microsoft.
 *
 * Suppose an arithmetic expression is given as a binary tree. Each leaf is an integer and each internal node is one of '+', '−', '∗', or '/'.
 *
 * Given the root to such a tree, write a function to evaluate it.
 *
 * For example, given the following tree:
 *
 *     *
 *   /  \
 *  +    +
 * / \  / \
 * 3  2  4  5
 * You should return 45, as it is (3 + 2) * (4 + 5).
 */



abstract class BinaryTree[+A] {
  def isEmpty: Boolean
  def isValid: Boolean
}

case object EmptyTree extends BinaryTree[Nothing] {
  def isEmpty = true
  def isValid = true
}

case class NonEmptyTree[A](
                            var data: A,
                            var left: BinaryTree[A],
                            var right: BinaryTree[A])
                          (implicit ord: Ordering[A]) extends BinaryTree[A] {
  def isEmpty = false
  def isValid: Boolean = {
    import ord._
    def isValidWith(f: A => Boolean, t: BinaryTree[A]): Boolean = t match {
      case NonEmptyTree(that, _, _) => f(that) && t.isValid
      case EmptyTree => true
    }
    isValidWith(data < _, left) && isValidWith(data > _, right)
  }
}