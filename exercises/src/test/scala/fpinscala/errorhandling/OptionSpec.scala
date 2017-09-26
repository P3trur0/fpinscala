package fpinscala.errorhandling

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by P3trur0 (https://flatmap.it) on 18/09/17.
  */
class OptionSpec extends FlatSpec with Matchers {

  "Excercise 4.1" should "implement a set of methods for Option" in {
    val elem: Option[Int] = Some(4)
    elem.map(_*2) shouldBe(Some(8))
    elem.flatMap(x => Some(x*10)) shouldBe Some(40)
    None.getOrElse(Some(100)) shouldBe Some(100)
    None.orElse(Some(1000)) shouldBe Some(1000)
    elem.filter(_>100) shouldBe None
  }

  "Exercise 4.2" should "calculate the variance of a sequence" in {
    Option.variance(List(1.0,2.0,4.0,5.0,7.0,8.0)) shouldBe Some(6.25)
  }

  "Exercise 4.3" should "lift a two arguments function" in {
    Option.map2(Some(10),Some(2))(_*_) shouldBe Some(20)
    Option.map2WithFor(Some(10),Some(2))(_*_) shouldBe Some(20)
    Option.map2WithFor[Int, Int, Int](Some(2),None)(_*_) shouldBe None
  }

  "Exercise 4.4" should "transform a list of options in an option of a list" in {
    Option.sequence(List(Some(1), Some(2), Some(3))) shouldBe Some(List(1,2,3))
    Option.sequence(List(Some(1), None, Some(3))) shouldBe None
    Option.sequence_traverse(List(Some(1), Some(2), Some(3))) shouldBe Some(List(1,2,3))
  }

  "Exercise 4.5" should "transform a list in an option of a list" in {
    import Option._

    Option.traverse(List("1", "2", "3"))(x => Try(x.toInt)) shouldBe Some(List(1,2,3))
    Option.traverse(List("1", "2", "xxx"))(x => Try(x.toInt)) shouldBe None
  }

}
