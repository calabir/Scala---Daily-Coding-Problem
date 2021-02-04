package dailyCodingProblem.dailyCodingProblem.hard.Pending.Problem59

object FileSync extends App {
  /** This problem was asked by Google.
   *
   * Implement a file syncing algorithm for two computers over a low-bandwidth network. What if we know the files in the two computers are mostly the same?
   * */

  //Idea
  //First we check if they are equal computing the checksum

  //If equal then nothing
  //If not equal:
  //  If names are the same: we compare the bytes of the BASE64
    // We overwrite the bytes that are not the same
  // Else the names are not equal
    // Compress
    // Send


  //TODO after googling this problem, it looks like we should use Merkle Trees, this problem is really out of my knowledge box
  //https://gist.github.com/saurabhgokhale/0161185ee6f454fb8a4238dd9bf43dc6#file-markletree-java

}
