package dailyCodingProblem.dailyCodingProblem.easy.Solved.Problem82

import scala.annotation.tailrec
import scala.io.Source

/**
 * This problem was asked Microsoft.
 *
 * Using a read7() method that returns 7 characters from a file, implement readN(n) which reads n characters.
 *
 * For example, given a file with the content “Hello world”, three read7() returns “Hello w”, “orld” and then “”.
 */
object Read7 extends App {
  val currentDirectory = new java.io.File(".").getCanonicalPath  + "\\src\\main\\scala\\dailyCodingProblem\\dailyCodingProblem\\easy\\Problem82"
  val bufferedSource = Source.fromFile(currentDirectory + "\\" + "test")
  val fileContents = bufferedSource.getLines.mkString

  //TODO find a more immutable way of doing this to avoid the vars
  var charsReadSoFar = 0

  def read7(in: String): String = {
    val out = in.slice(charsReadSoFar, charsReadSoFar + 7)
    charsReadSoFar = charsReadSoFar + out.length
    out
  }

//  println(read7(fileContents))
//  println(read7(fileContents))
//  println(read7(fileContents))

  var readBuffer = ""
  var charsReadN = 0

  @tailrec
  def readN(n: Int, in: String): String = {
    if(readBuffer.length >= n){
      val res = readBuffer.take(n)
      readBuffer = readBuffer.drop(n)
      res
    }else{
      readBuffer = readBuffer + read7(in)
      readN(n, in)
    }
  }

  println(readN(3, fileContents))
  println(readN(3, fileContents))
  println(readN(3, fileContents))

  bufferedSource.close

}
