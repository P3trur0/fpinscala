package fpinscala.parallelism

import org.scalatest.{FlatSpec, Matchers}
import java.util.concurrent.{Callable, ExecutorService, Executors}

class ParallelismSpec extends FlatSpec with Matchers {

  "Exercise 7.2" should "implement a thread workflow" in {
    def fnSum = (x: Int, y: Int) => x + y
    val operationOne = ParTest.unit(fnSum(3, 4))
    val operationTwo = ParTest.unit(fnSum(8, 9))
    val result = ParTest.map2(operationOne, operationTwo)(_ + _)
    result.get shouldBe 24
  }

  "Exercise 7.4" should "implement an async function execution" in {
    def asyncConverter = Par.asyncF((a: Int) => s"Hello, ${a.toString}")
    lazy val executor: ExecutorService = Executors.newScheduledThreadPool(10)
    Par.run(executor)(asyncConverter(10)).get shouldBe "Hello, 10"
  }
}

//This is my Exercise 7.2, almost similar to actual considerations did in the chapter
class ParTest[A](a: A) {
  val element: A = a
  def get : A = element
}

object ParTest {
  import Runner._

  def unit[A](a: A): ParTest[A] = new ParTest[A](a)
  def map2[A,B,C](parA: ParTest[A], parB: ParTest[B])(f: (A,B) => C): ParTest[C] = unit(f(run(parA), run(parB)))
  def fork[A](a: => ParTest[A]): ParTest[A] = a
  def forkUnit[A](a: => A): ParTest[A] = fork(unit(a))
  def run[A](a: ParTest[A]): A = mapRunner(new CallablePar(a))(getValue)
}

object Runner {
  import java.util.concurrent.Executors
  import java.util.concurrent.ExecutorService
  import java.util.concurrent.Future

  lazy val executor: ExecutorService = Executors.newScheduledThreadPool(10)

  def runService[A](callable: Callable[A]): Future[A] = {
    executor.submit(callable)
  }
  def getValue[A](future: Future[A]): A = future.get()

  def mapRunner[A](callable: Callable[A])(f: Future[A] => A): A = {
      f(runService(callable))
  }

}

class CallablePar[A](val parTest: ParTest[A]) extends Callable[A] {
  override def call(): A = {
   // println(Thread.currentThread())
    parTest.element
  }
}