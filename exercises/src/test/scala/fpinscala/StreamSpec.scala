package fpinscala


import org.scalatest.{FlatSpec, Matchers}
import fpinscala.laziness.{Empty, Stream}

/**
  * Created by P3trur0 (https://flatmap.it) on 25/09/17.
  */
class StreamSpec extends FlatSpec with Matchers {

  "Exercise 5.1" should "transform a stream to a list" in {
    val stream: Stream[Int] = Stream(1, 2, 3)
    stream.toList shouldBe List(1,2,3)
  }

  "Exercise 5.2" should "implement both take and drop" in {
    val stream: Stream[Int] = Stream(1, 2, 3, 4, 5, 6)
    stream.take(3) shouldBe Stream(1, 2, 3)
    stream.take(0) shouldBe Empty
    stream.drop(2) shouldBe Stream(3, 4, 5, 6)
    stream.drop(0) shouldBe stream
  }

  "Exercise 5.3" should "implement a takeWhile" in {
    val stream: Stream[Int] = Stream(1, 2, 3, 4, 5, 6)
    stream.takeWhile(_<=3) shouldBe Stream(1, 2, 3)
    stream.takeWhile(_>0) shouldBe Stream(1, 2, 3, 4, 5, 6)
    stream.takeWhile(_<0) shouldBe Stream()
  }

}
