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
    stream.take(3).toList shouldBe List(1, 2, 3)
    stream.take(0).toList shouldBe Nil
    stream.drop(2).toList shouldBe List(3, 4, 5, 6)
    stream.drop(0).toList shouldBe stream.toList
  }

  "Exercise 5.3" should "implement a takeWhile" in {
    val stream: Stream[Int] = Stream(1, 2, 3, 4, 5, 6)
    stream.takeWhile(_<=3).toList shouldBe Stream(1, 2, 3).toList
    stream.takeWhile(_>0).toList shouldBe Stream(1, 2, 3, 4, 5, 6).toList
    stream.takeWhile(_<0).toList shouldBe Nil
  }

  "Exercise 5.4" should "implement forAll" in {
    val stream: Stream[Int] = Stream(2, 4, 6, 8, 10, 12)
    val oddStream: Stream[Int] = Stream(2, 4, 6, 1, 10, 12)

    def allEven(str: Stream[Int]): Boolean = str forAll(_%2==0)

    allEven(stream) shouldBe true
    allEven(oddStream) shouldBe false
  }

  "Exercise 5.5" should "implement a takeWhile" in {
    val stream: Stream[Int] = Stream(1, 2, 3, 4, 5, 6)
    stream.takeWhileWithFold(_<=3).toList shouldBe Stream(1, 2, 3).toList
    stream.takeWhileWithFold(_>0).toList shouldBe Stream(1, 2, 3, 4, 5, 6).toList
    stream.takeWhileWithFold(_<0).toList shouldBe Stream().toList
  }

  "Exercise 5.6" should "implement headOption using foldRight" in {
    Stream(1,2,3,4).headOption shouldBe Some(1)
    Stream().headOption shouldBe None
  }

  "Exercise 5.7" should "implement map, flatMap and append" in {
    Stream(1,2,3,4).map(_*2).toList shouldBe List(2,4,6,8)
    Stream(1, 2, 3, 4).flatMap(x => Stream(x,x)).toList shouldBe List(1,1,2,2,3,3,4,4)
    Stream(1,2,3,4).append(Stream(5,6,7,8)).toList shouldBe (1 to 8).toList
    Stream(1,2,3,4).filter(_%2==0).toList shouldBe List(2,4)
  }

}
