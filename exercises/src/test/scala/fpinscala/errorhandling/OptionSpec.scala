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

}
