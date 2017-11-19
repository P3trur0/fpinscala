package fpinscala.state;


import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by P3trur0 (https://flatmap.it) on 04/10/17.
  */
class StateSpec extends FlatSpec with Matchers {

  "Exercise 6.1" should "always return a random number equal or greater than zero" in {
    val rng = RNG.Simple(42)
    val (result, nextRNG) = RNG.nonNegativeInt(rng)
    result should be >= 0
    RNG.nonNegativeInt(nextRNG)._1 should be >= 0
  }

  "Exercise 6.2" should "always return a double between 0 and 1 random number" in {
    val rng = RNG.Simple(42)
    val (result, nextRNG) = RNG.double(rng)
    result should (be >= 0d and be < 1d)
  }

  "Exercise 6.3" should "implement a set of utility functions" in {
    val rng = RNG.Simple(42)
    val ((intVal, doubleVal),rngNext) = RNG.intDouble(rng)
    doubleVal should (be >= 0d and be < 1d)

    val ((doubleVal1, intVal1),rngNextNext) = RNG.doubleInt(rngNext)
    doubleVal1 should (be >= 0d and be < 1d)

    val ((d1, d2, d3), rngNextNextNext) = RNG.double3(rngNextNext)
    d1 should (be >= 0d and be < 1d)
    d2 should (be >= 0d and be < 1d)
    d3 should (be >= 0d and be < 1d)
  }

  "Exercise 6.4" should "return a list of random integers" in {
    val (r1,next) = RNG.Simple(42).nextInt
    val (r2,next1) = next.nextInt
    val (r3,next2) = next1.nextInt
    val (r4,_) = next2.nextInt

    val (ls, _) = RNG.ints(4)(RNG.Simple(42))
    ls.size shouldBe 4
    //Here the check is done in reverse order since the list is built in tailrec fashion
    ls(0) shouldBe r4
    ls(1) shouldBe r3
    ls(2) shouldBe r2
    ls(3) shouldBe r1
  }

  "Exercise 6.5" should "implement an elegant double using state transitions" in {
    val (r1,_) = RNG.elegantDouble(RNG.Simple(42))
    r1 shouldBe a [java.lang.Double]
  }

  "Exercise 6.6" should "implement map 2" in {
    val r = RNG.map2(RNG.int, RNG.elegantDouble)((a,b) => (a,b))(RNG.Simple(42))
    r shouldBe a [((java.lang.Integer, java.lang.Double), RNG)]
  }

  "Exercise 6.7" should "implement a sequence operation" in {
    import RNG._
    val integers = sequencedInts(3)(Simple(42))
    integers.size shouldBe 3
  }

  "Exercise 6.11" should "implement a vendor machine" in {

    val machine = Machine(locked  = true, candies = 5, coins = 10)
    val input = List(Coin, Coin, Coin, Coin)

    State.set(machine)
    val result = State.simulateMachine(input)
    val ((coins, candies), _) = result.run(State.get)

    coins shouldBe 14
    candies shouldBe 1

  }

}
