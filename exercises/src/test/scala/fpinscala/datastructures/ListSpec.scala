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
    List.dropWhile(List(1,2,3,4,5), (x: Int) => x < 4) shouldBe(List(4,5))
  }

  it should "return Nil if the dropWhile method removes all the elements of the list" in {
    List.dropWhile(List(1,2,3,4,5), (x: Int) => x < 10) shouldBe(Nil)
  }

}
