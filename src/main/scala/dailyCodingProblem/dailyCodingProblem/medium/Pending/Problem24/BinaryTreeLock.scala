package dailyCodingProblem.dailyCodingProblem.medium.Pending.Problem24

/**
 * This problem was asked by Google.
 *
 * Implement locking in a binary tree. A binary tree node can be locked or unlocked only if all of its descendants or ancestors are not locked.
 *
 * Design a binary tree node class with the following methods:
 *
 * is_locked, which returns whether the node is locked
 * lock, which attempts to lock the node. If it cannot be locked, then it should return false. Otherwise, it should lock it and return true.
 * unlock, which unlocks the node. If it cannot be unlocked, then it should return false. Otherwise, it should unlock it and return true.
 * You may augment the node to add parent pointers or any other property you would like. You may assume the class is used in a single-threaded program, so there is no need for actual locks or mutexes. Each method should run in O(h), where h is the height of the tree.
 * */

//TODO Add a pointer to the parent and also check it for locking/unlocking (all the parents)
object BinaryTreeLock extends App {

  def iterateTree(node: element): String = {
    if (!node.isNull) node.value + "Left: " + iterateTree(node.left) + "Right: " + iterateTree(node.right) + "\n"
    else ""
  }

  def allChildUnlocked(node: element): Boolean = {
    if (node.isLocked) {
      println("The node: " + node.value + " is locked: " + node.isLocked)
      false}
    else {
      val leftBool = if (!node.left.isNull) allChildUnlocked(node.left) else true
      val rightBool = if (!node.right.isNull) allChildUnlocked(node.right) else true
      leftBool && rightBool
    }
  }

  trait element {
    val value: Int
    val left: element
    val right: element
    val isNull: Boolean
    var isLocked: Boolean

    def is_locked(): Boolean

    def lock(): Boolean

    def apply(): element
  }

  class node(val value: Int, val left: element, val right: element, var isLocked: Boolean = false) extends element {
    val isNull: Boolean = false

    def is_locked(): Boolean = isLocked

    def lock(): Boolean = if (allChildUnlocked(this) && !this.isLocked) {
      this.isLocked = true
      true
    } else false

    def unlock(): Boolean = if(allChildUnlocked(this) && this.isLocked){
      this.isLocked = false
      true
    }else false

    def apply(): element = new node(value, left, right)


    override def toString: String = iterateTree(this)
  }


  class leaf(val value: Int, var isLocked: Boolean = false) extends element {
    val left: element = nullNode
    val right: element = nullNode

    val isNull: Boolean = false

    def is_locked(): Boolean = isLocked

    def lock(): Boolean = {
      println("LEAF: The node: " + this.value + " when enters the lock function is: " + this.isLocked)
      if (!this.isLocked) {
      this.isLocked = true
      true
    } else false
    }

    def unlock(): Boolean = if(this.isLocked){
      this.isLocked = false
      true
    }else false

    def apply(): element = new leaf(value, isLocked)
  }

  object nullNode extends element {
    val value: Int = 0
    val left: element = this
    val right: element = this
    val isNull: Boolean = true
    var isLocked: Boolean = false

    def is_locked(): Boolean = throw new Exception("A null node can't be locked or unlocked")

    def lock(): Boolean = throw new Exception("A null node can't be locked or unlocked")

    def apply(): element = nullNode
  }



  val n1 = new node(1, new node(2, nullNode, nullNode), new node(3, nullNode, nullNode))
  val n2 = new node(1, new leaf(2), new leaf(3))
  val leaf2 = new leaf(2, true)
  val n3 = new node(1, leaf2, new leaf(3))


  println(iterateTree(n1))
  println("-------------")
  println(iterateTree(n2))

//  assert(allChildUnlocked(n1))
//  assert(!allChildUnlocked(n3))


  val leaf3 = new leaf(3, true)
  val leaf4 = new leaf(4, false)
  val leaf5 = new leaf(4, false)
  val n4 = new node(1, leaf3, new leaf(5))
  val n5 = new node(1, leaf5, new leaf(6))
  assert(!leaf3.lock())
  assert(leaf4.lock())
  assert(!n4.lock())

  assert(!n5.unlock())
  assert(n5.lock())
  //TODO the following unlock is not working
//  assert(n5.unlock())



}
