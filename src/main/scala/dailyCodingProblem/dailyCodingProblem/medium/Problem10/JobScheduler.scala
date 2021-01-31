package dailyCodingProblem.dailyCodingProblem.medium.Problem10

object JobScheduler extends App {


  /**
   * This problem was asked by Apple.
   *
   * Implement a job scheduler which takes in a function f and an integer n, and calls f after n milliseconds.
   */


  class myThread(n: Int)(f: => Unit) extends Runnable {
    override def run() {
      scheduleThis(n)(f)
    }

    def scheduleThis(n: Int)(f: => Unit): Unit = {
      Thread.sleep(n)
      f
    }

  }

  //new myThread(1000)(println("Hola, ha passat 1 segon"))


  new Thread(new myThread(10000)(println("10 seconds"))).start()
  new Thread(new myThread(1000)(println("1 seconds"))).start()
  new Thread(new myThread(8000)(println("8 seconds"))).start()
  new Thread(new myThread(2000)(println("2 seconds"))).start()
}
