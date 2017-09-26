package fpinscala.errorhandling

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by P3trur0 (https://flatmap.it) on 23/09/17.
  */
class EitherSpec extends FlatSpec with Matchers {

  "Exercise 4.6" should "implement all the functions of either" in {
      val elem: Either[String, Int] = Right(4)
      elem.map(_*2) shouldBe(Right(8))
      elem.flatMap(x => Left("error")) shouldBe Left("error")
      elem.orElse(Left("error left")) shouldBe Right(4)
      Left("error left").orElse(elem) shouldBe Right(4)
      elem.map2(Right(8))(_ + _) shouldBe Right(12)
  }

  "Exercise 4.7" should "implement traverse and sequence" in {
      import Either.Try
      Either.traverse(List("1","2","3"))(x => Try(x.toInt)) shouldBe Right(List(1,2,3))
      Either.traverse(List("1","2","3xx"))(x => Try(x.toInt)) shouldBe a [Left[_]]
    Either.sequence(List(Right(1), Right(2), Right(3))) shouldBe Right(List(1,2,3))
  }

}
