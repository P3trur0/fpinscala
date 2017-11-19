package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {

  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt // r => r.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (num, next) = rng.nextInt
    if (num < 0) (-(num + 1), next) else (num, next)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (i, next) = nonNegativeInt(rng)
    (i / (Int.MaxValue.toDouble + 1), next)
  }

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (integer, next) = rng.nextInt
    val (dbl, nextNext) = double(next)
    ((integer, dbl), nextNext)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((integer, dbl), next) = intDouble(rng)
    ((dbl, integer), next)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (dbl, next) = double(rng)
    val (dbl2, nextNext) = double(next)
    val (dbl3, nextNextNext) = double(nextNext)
    ((dbl, dbl2, dbl3), nextNextNext)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    @annotation.tailrec
    def loop(counter: Int, nextRng: RNG, ls: List[Int]): (List[Int], RNG) = {
      if (counter <= 0) (ls, nextRng)
      else {
        val (i, next) = nextRng.nextInt
        loop(counter - 1, next, i :: ls)
      }
    }

    loop(count, rng, Nil)

  }

  def nonNegativeEven: Rand[Int] = {
    map(nonNegativeInt)(i => i - i % 2)
  }

  def elegantDouble: Rand[Double] = {
    map(nonNegativeInt)(i => i / (Int.MaxValue.toDouble + 1))
  }


  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (a, rng2) = ra(rng)
      val (b, rng3) = rb(rng2)
      (f(a, b), rng3)
    }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] = {
    @annotation.tailrec
    def loop(input: List[Rand[A]], output: Rand[List[A]]): Rand[List[A]] = {
      input match {
        case Nil => output
        case h :: Nil => map2(output, h)((list, element) => element :: list)
        case h :: t => loop(t, map2(output, h)((list, element) => element :: list))
      }
    }

    loop(fs, RNG.unit(Nil))
  }

  def sequencedInts(size: Int)(rng: RNG): List[Int] = {
    val executionList = (0 until size).map(i => int).toList
    sequence(executionList)(rng)._1
  }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }
  }

  def nonNegativeLessThan(n: Int): Rand[Int] =
    flatMap(nonNegativeInt)(i => {
      val mod = i % n
      if (i + (n - 1) - mod >= 0) unit(mod)
      else nonNegativeLessThan(n)
    })


  def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] = {
    rng => {
      val (a, rng2) = f(rng)
      g(a)(rng2)
    }
  }

  def mapWithFlatMap[A,B](f: Rand[A])(g: A => B): Rand[B] = {
    flatMap(f)(a => unit(g(a)))
  }

  def mapWithFlatMap2[A,B,C](r1: Rand[A], r2: Rand[B])(g: (A, B) => C): Rand[C] = {
    //flatMap(r1)(a => unit(g(a, flatMap(r2)(b => unit(unit(b))))))
    flatMap(r1)(a => map(r2)(b => g(a,b)))
  }
}

case class State[S,+A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] = State(
    s => {
      val (a, newState) = this.run(s)
      (f(a), newState)
    }
  )

  def map2[B,C](sb: State[S, B])(f: (A, B) => C): State[S, C] = State(
    s => {
      val (a, newState) = this.run(s)
      val (b, finalState) = sb.run(newState)
      (f(a, b), finalState)
    }
  )

  def flatMap[B](f: A => State[S, B]): State[S, B] = State(
    s => {
      val (a, state): (A, S) = this.run(s)
     f(a).run(state)
    }
  )
}

sealed trait Input
case object Coin extends Input
case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def get[S]: State[S, S] = State(s => (s, s))
  def set[S](s: S): State[S, Unit] = State(_ => ((), s))

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = {

   

  }

  def unit[S, A](a: A): State[S, A] = State(s => (a,s))

  def sequence[S, A](fs: List[State[S,A]]): State[S, List[A]] = {
    fs.foldRight[State[S, List[A]]](State.unit(Nil))((elem, acc) => elem.map2(acc)((a,b)=>a::b))
  }
}
