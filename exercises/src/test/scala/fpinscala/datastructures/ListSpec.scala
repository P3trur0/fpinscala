package fpinscala.datastructures
import org.scalatest._
/**
  * Created by P3trur0 on 05/09/17.
  */
class ListSpec extends FlatSpec with Matchers {

  import fpinscala.datastructures.List

  "Exercise 3.2" should "retrieve a tail of a list if not empty" in {
    List.tail(List(1,2,3,4,5)) shouldBe(List(2,3,4,5))
  }

  it should "throw a system error if the list is empty" in {
    assertThrows[RuntimeException] {
      List.tail(List())
    }
  }


  "Exercise 3.3" should "allow to change the head of a list" in {
      List.setHead(List(1,2,3,4,5), 999) shouldBe(List(999, 2,3,4,5))
    }

   it should "throw a system error if the list empty" in {
      assertThrows[RuntimeException] {
        List.setHead(List(), 999)
      }
  }

  "Exercise 3.4" should "allow to change the drop n elements of a list" in {
    List.drop(List(1,2,3,4,5), 3) shouldBe(List(4,5))
  }

  it should "return Nil if the drop elements to remove are greater than the length of the list" in {
    List.drop(List(1,2,3,4,5), 8) shouldBe(Nil)
  }

  "Exercise 3.5" should "allow to drop elements of a list while a boolean condition is met" in {
    List.dropWhile(List(1,2,3,4,5))(x => x < 4) shouldBe(List(4,5))
  }

  it should "return Nil if the dropWhile method removes all the elements of the list" in {
    List.dropWhile(List(1,2,3,4,5))(x => x < 10) shouldBe(Nil)
  }


  "Exercise 3.6" should "return all the list but the last element" in {
    List.init(List(1,2,3,4,5)) shouldBe(List(1,2,3,4))
  }

  it should "return Nil if the init method applies on a single element list" in {
    List.init(List(1)) shouldBe(Nil)
  }

  it should "throw an exception if the init method applies on an empty list" in {
    assertThrows[RuntimeException] {
      List.init(Nil) shouldBe(Nil)
    }
  }

  "Exercise 3.9" should "return the length of a list" in {
    List.length(List(1,2,3,4,5)) shouldBe 5
    List.length(List(2,3,4,5)) shouldBe 4
    List.length(List(1,2,3)) shouldBe 3
  }

  it should "return 0 when the list is empty" in {
    List.length(List()) shouldBe 0
  }

  "Exercise 3.10" should "return the foldLeft of a list" in {
    List.foldLeft(List(1,2,3,4,5), 0)(_ + _) shouldBe 15
  }

  "Exercise 3.11" should "return the list product and sum using the fold left" in {
    List.sumFoldedLeft(List(1,2,3,4,5)) shouldBe 15
    List.productFoldedLeft(List(2,4,6,8)) shouldBe 384
  }

  "Exercise 3.12" should "return the reverse representation of the list" in {
    List.reverse(List(1,2,3,4,5)) shouldBe List(5,4,3,2,1)
    List.reverse(List(1)) shouldBe List(1)
    List.reverse(List()) shouldBe Nil
  }

  "Exercise 3.14" should "return an appended version of two lists" in {
    List.appendWithFold(List(1,2), List(3,4,5)) shouldBe List(1,2,3,4,5)
  }

  "Exercise 3.15" should "return a concat of a list of lists" in {
    List.concat(List(List(1,3,4), List(2,5))) shouldBe List(1,3,4,2,5)
    List.concatWithAppend(List(List(1,3,4), List(2,5))) shouldBe List(1,3,4,2,5)
  }

  "Exercise 3.16" should "increase a list of integers by one" in {
    List.increaseByOne(List(1,2,3,4)) shouldBe List(2,3,4,5)
  }

  "Exercise 3.17" should "transform a list of Doubles in a list of Strings" in {
    List.doubleToString(List(1.0,2.0,3.0,4.0)) shouldBe List("1.0","2.0","3.0","4.0")
  }

  "Exercise 3.18" should "implement a map using fold" in {
    List.map(List(1.0,2.0,3.0,4.0))(_.toString) shouldBe List("1.0","2.0","3.0","4.0")
  }

  "Exercise 3.19" should "implement a filter using fold" in {
    List.filter(List(1.0,2.0,3.0,4.0))(_%2==0) shouldBe List(2.0,4.0)
  }

  "Exercise 3.20" should "implement a flatmap" in {
    List.flatMap(List(1,2,3))(i => List(i,i)) shouldBe List(1,1,2,2,3,3)
    List.flatMapWithConcat(List(1,2,3))(i => List(i,i)) shouldBe List(1,1,2,2,3,3)
  }

  "Exercise 3.21" should "implement a filter using flatMap" in {
    List.filterWithFlatMap(List(1.0,2.0,3.0,4.0))(_%2==0) shouldBe List(2.0,4.0)
  }

  "Excercise 3.22" should "implement a zip with sum for lists of integers" in {
    List.zipSumIntegerLists(List(1,2,3), List(4,5,6)) shouldBe List(5,7,9)
  }

  "Excercise 3.23" should "implement a zip with sum for lists of integers" in {
    List.zipLists(List("hello, ","ciao, ", "bonjour, "), List("world!", "mondo!","monde!"))(_ + _) shouldBe List("hello, world!", "ciao, mondo!", "bonjour, monde!")
  }

  "Excerise 3.24" should "check if a list has a specific sub sequence" in {
    List.hasSubsequence(List(1,2,3,4), List(2,3)) shouldBe true
    List.hasSubsequence(List(1,2,3,4), List(1,2,3)) shouldBe true
    List.hasSubsequence(List(1,2,3,4), List(4,3)) shouldBe false
    List.hasSubsequence(List(1,2,3,4), List(4)) shouldBe true
  }
}
