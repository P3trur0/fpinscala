package fpinscala.datastructures

sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing] // A `List` data constructor representing the empty list
/* Another data constructor, representing nonempty lists. Note that `tail` is another `List[A]`,
which may be `Nil` or another `Cons`.
 */
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List { // `List` companion object. Contains functions for creating and working with lists.
  def sum(ints: List[Int]): Int = ints match { // A function that uses pattern matching to add up a list of integers
    case Nil => 0 // The sum of the empty list is 0.
    case Cons(x,xs) => x + sum(xs) // The sum of a list starting with `x` is `x` plus the sum of the rest of the list.
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x,xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1,2,3,4,5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h,t) => Cons(h, append(t, a2))
    }

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x,y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar


  def tail[A](l: List[A]): List[A] = {
    l match {
      case Nil => sys.error("No tail in empty list")
      case Cons(_, t) => t
    }
  }

  def setHead[A](l: List[A], h: A): List[A] = {
    l match {
      case Nil => sys.error("impossible to set head to nil function")
      case Cons(_, t) => Cons(h, t)
    }

  }

  def drop[A](l: List[A], n: Int): List[A] = {
    l match {
      case Nil => Nil
      case Cons(_,t) if n > 0 => drop(t, n-1)
      case _ => l
    }
  }

  def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = {
    l match {
      case Nil => Nil
      case Cons(h,t) if f(h) => dropWhile(t)(f)
      case _ => l
    }
  }

  def init[A](l: List[A]): List[A] = {
    def loop(l: List[A], result: List[A]): List[A] = {
      l match {
        case Nil => sys.error("Init of an empty list!")
        case Cons(h, Nil) => result
        case Cons(h, Cons(t, Nil)) => Cons(h, result)
        case Cons(h, t) => loop(t, Cons(h, result))
      }
    }

    def reverse(l: List[A], result: List[A]): List[A] = {
      l match {
        case Nil => result
        case Cons(h, Nil) => Cons(h, Nil)
        case Cons(h, Cons(t, Nil)) => Cons(t, Cons(h, result))
        case Cons(h, t) => reverse(t, Cons(h, result))
      }
    }

    reverse(loop(l, Nil), Nil)
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_,b) => b + 1)

  def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z,h))(f)
  }

  def sumFoldedLeft(l: List[Int]): Int = {
    foldLeft(l, 0)(_ + _)
  }

  def productFoldedLeft(l: List[Int]): Int = {
    foldLeft(l, 1)(_ * _)
  }

  def reverse[A](l: List[A]): List[A] = {
    foldLeft(l, Nil:List[A])((b, a) => Cons(a, b))
  }

  def appendWithFold[A](a1: List[A], a2: List[A]): List[A] = {
    foldRight(a1, a2)((a,b) => Cons(a, b))
  }

  def concat[A](l: List[List[A]]): List[A] = {
    foldRight(l, Nil:List[A])((a,b) => foldRight(a, b)((c,d) => Cons(c,d)))
  }

  def concatWithAppend[A](l: List[List[A]]): List[A] = {
    foldRight(l, Nil:List[A])(append)
  }

  def increaseByOne(l: List[Int]): List[Int] = {
    foldRight(l, Nil:List[Int])((a,b) => Cons(a+1,b))
  }

  def doubleToString(l: List[Double]): List[String] = {
    foldRight(l, Nil:List[String])((a,b) => Cons(a.toString,b))
  }

  def map[A,B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil:List[B])((a,b) => Cons(f(a),b))

  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, Nil:List[A]) {
    (a,b) =>
      if(f(a))
        Cons(a,b)
      else
        b
  }

  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    foldRight(as, Nil:List[B])((a,b) => append(f(a),b))
  }

  def flatMapWithConcat[A,B](l: List[A])(f: A => List[B]): List[B] = {
    val value = map(l)(f)
    concat(value)
  }

  def filterWithFlatMap[A](l: List[A])(f: A => Boolean): List[A] = {
    def evaluator[A](a: A, f: A => Boolean): List[A] = {
      if(f(a))
        Cons(a, Nil)
      else
        Nil
    }

    flatMapWithConcat(l)(a => evaluator(a,f))
  }


  def zipLists[A](a1: List[A], a2: List[A])(f: (A,A) => A): List[A] = {
    import collection.mutable.ListBuffer
    val buf: ListBuffer[A] = new ListBuffer
    @annotation.tailrec
    def loop(a1: List[A], a2: List[A]): Unit =
      (a1,a2) match {
      case (_,Nil) => ()
      case (Nil,_) => ()
      case (Cons(h1,t1), Cons(h2,t2)) => buf+=f(h1,h2); loop(t1,t2)
    }

    loop(a1, a2)
    List(buf.toList: _*)
  }

  def zipSumIntegerLists(a1: List[Int], a2: List[Int]): List[Int] = {
    zipLists(a1, a2)(_ + _)
  }
}
