val array1 = Array(5, 4, 3, 2, 1)

def compareValues(i: Int, j: Int): Int = {
  if (i < j) 1
  else 0
}



def firstApproach(array1: Array[Int]): Int = {
  val pairs = for {
    i <- array1
    j <- array1.tail
  } yield { //println((i,j))
    compareValues(i, j)
  }

  println("Input: " + array1.length)
  println("Output: " + pairs.length)

  println("Proportion: " + pairs.length / array1.length)
  (pairs foldRight 0) ((x, y) => x + y)

}

val array01 = Array(1)
val array2 = Array(1, 2)
val array3 = Array(1, 2, 3)
val array4 = Array(1, 2, 4)
val array5 = Array(1, 2, 4, 5)
val array6 = Array(1, 2, 4, 5, 6)


firstApproach(array01)
firstApproach(array2)
firstApproach(array3)
firstApproach(array4)
firstApproach(array5)
firstApproach(array6)

//Complexity o((n-1)*n))
//if n = 6 -> 5*6 = 30. Is better than O(n^2) which is 36 lol
//TODO How can we improve the complexity, we don't really want to compute all vector pairs

val array7 = Array(6, 3, 2, 5, 7)
