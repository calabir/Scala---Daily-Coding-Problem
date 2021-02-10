import org.junit.Assert.assertEquals

/** This problem was asked by Google.
 *
 * The area of a circle is defined as πr^2. Estimate π to 3 decimal places using a Monte Carlo method.
 *
 * Hint: The basic equation of a circle is x2 + y2 = r2.
 *
 * More info: https://en.wikipedia.org/wiki/Monte_Carlo_method
 */

case class Point(i: Int, j: Int){
  def getI() = this.i
  def getJ() = this.j
}

//It takes 1 second to compute it using this range. The number of points is this value pow(2)
val range = 4000
val origin = Point(0, 0)

def distancePointToCenter(p1: Point) = {
  Math.sqrt(math.pow(p1.j - origin.j, 2) + math.pow(p1.i - origin.i, 2))
}


def findPi(): Double = {
  val before = System.currentTimeMillis()
  println(before)
  val iRange = 0 to range by 1
  val jRange = 0 to range by 1

  val allPoints = for(i<-iRange; j<-iRange) yield Point(i, j)

  val allPointsDistanceLte1 = allPoints.map(p => if(distancePointToCenter(p) <= range) 1 else 0).sum.toDouble

  val pi = (allPointsDistanceLte1/(iRange.length * jRange.length))*4
  println(pi)

  val after = System.currentTimeMillis()
  println(after)
  println("Elapsed time to compute Pi using: " + range*range + "random points was: " + (after-before)/1000 + "seconds")
  pi
}


assertEquals(range, distancePointToCenter(Point(range, 0)), 0.001)
assertEquals(range, distancePointToCenter(Point(0, range)), 0.001)
assertEquals(3.141, findPi(), 0.001)


