import java.lang.Thread.sleep

/**
 * This problem was asked by Apple.
 *
 * Implement a job scheduler which takes in a function f and an integer n, and calls f after n milliseconds.
 */


def scheduleThis(n: Int)(f: => Unit): Unit = {
  println(System.currentTimeMillis())
  sleep(n)
  println(System.currentTimeMillis())
 f
}

//scheduleThis(1000)(println("Hola, ha passat 1 segon"))
//scheduleThis(8000)(println("Hola, han passat 8 segons"))
//scheduleThis(2000)(println("Hola, ha passat 2 segon"))
//
//

def addTwo(in: List[Int]): List[Int] = {
  in.map(_ + 2)
}

//addTwo(List(1,2,3))

//scheduleThis(5000)(addTwo(List(1,2,3)))



class myThread(n: Int)(f: => Unit) extends Runnable  {
  def scheduleThis(n: Int)(f: => Unit): Unit = {
    println(System.currentTimeMillis())
    Thread.sleep(100)
    println("aixo no ho mostra")
    println(System.currentTimeMillis())
    f
  }

  override def run() {
    println("hola soc un thread")
//    scheduleThis(n)(f)
    println(System.currentTimeMillis())
    Thread.sleep(n)
    println("aixo no ho mostra")
    println(System.currentTimeMillis())
    f
  }

}

//new myThread(1000)(println("Hola, ha passat 1 segon"))

new Thread(new myThread(1000)(println("Hola, ha passat 1 segon"))).start()
new Thread(new myThread(8000)(println("Hola, han passat 8 segons"))).start()
new Thread(new myThread(2000)(println("Hola, ha passat 2 segon"))).start()

