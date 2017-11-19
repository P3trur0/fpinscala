package fpinscala.laziness

import org.scalatest.{FlatSpec, Matchers}

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

  "Exercise 5.8" should "implement a constant infinite stream" in {
    Stream.constant(3).take(4).toList shouldBe List(3,3,3,3)
  }

  "Exercise 5.9" should "implement a from infinite stream" in {
    Stream.from(3).take(4).toList shouldBe List(3,4,5,6)
  }

  "Exercise 5.10" should "implement an infinite Fibonacci calculator" in {
    Stream.fibs.take(6).toList shouldBe List(0,1,1,2,3,5)
  }

  "Exercise 5.12" should "implement previous functions with unfold" in {
    Stream.constantUnfold(3).take(4).toList shouldBe List(3,3,3,3)
    Stream.fromUnfold(3).take(4).toList shouldBe List(3,4,5,6)
    Stream.onesUnfold.take(4).toList shouldBe List(1,1,1,1)
    Stream.fibsUnfold.take(6).toList shouldBe List(0,1,1,2,3,5)
    Stream("hello, ","ciao, ", "bonjour, ").zipWith(Stream("world!", "mondo!","monde!"))(_ + _).toList() shouldBe List("hello, world!", "ciao, mondo!", "bonjour, monde!")
    Stream(1, 2, 3, 4).zipAll(Stream(5, 6, 7)).toList() shouldBe List((Some(1),Some(5)),(Some(2),Some(6)),(Some(3),Some(7)),(Some(4), None))
  }

  "Exercise 5.13" should "implement map, take, takeWhile, zipWith and zipAll using fold" in {
    Stream(1,2,3,4).mapWithUnfold(elem => elem*2).toList shouldBe List(2,4,6,8)
    Stream(1, 2, 3, 4, 5, 6).takeWithUnfold(3).toList shouldBe List(1, 2, 3)
    Stream(1, 2, 3, 4, 5, 6).takeWhileWithUnfold(_<=3).toList shouldBe Stream(1, 2, 3).toList
    Stream(1, 2, 3, 4, 5, 6).takeWhileWithUnfold(_>0).toList shouldBe Stream(1, 2, 3, 4, 5, 6).toList
  }

  "Exercise 5.14" should "implement startsWith" in {
    Stream(1, 2, 3, 4, 5, 6).startsWith(Stream(1,2,3)) shouldBe true
    Stream(1, 2, 3, 4, 5, 6).startsWith(Stream(4,5,6)) shouldBe false
  }

  "Exercise 5.15" should "implement tails" in {
    val result = Stream(1,2,3).tails.toList
    result(0).toList shouldBe Stream(1,2,3).toList
    result(1).toList shouldBe Stream(2,3).toList
    result(2).toList shouldBe Stream(3).toList
    result(3).toList shouldBe Stream().toList
  }

  "Has sequence in lazy fashion" should "implement an has sequence operation" in {
    Stream(1, 2, 3, 4, 5, 6).hasSubsequence(Stream(1,2,3,4)) shouldBe true
    Stream(1, 2, 3, 4, 5, 6).hasSubsequence(Stream(2,3,4)) shouldBe true
    Stream(1, 2, 3, 4, 5, 6).hasSubsequence(Stream(3,4)) shouldBe true
    Stream(1, 2, 3, 4, 5, 6).hasSubsequence(Stream(1,4,3)) shouldBe false
  }

  "Exercise 5.16" should "implement scanRight" in {
    Stream(1, 2, 3).scanRight(0)(_+_).toList shouldBe List(6,5,3,0)
  }

  "My scanright version " should "be worse than the author's one" in {
    Stream(1, 2, 3).scanRightMyVersion(0)(_ + _).toList shouldBe List(6,5,3,0)
  }

}
