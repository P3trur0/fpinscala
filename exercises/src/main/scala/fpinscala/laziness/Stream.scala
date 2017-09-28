package fpinscala.laziness

import Stream._

trait Stream[+A] {

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h,t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean = 
    foldRight(false)((a, b) => p(a) || b) // Here `b` is the unevaluated recursive step that folds the tail of the stream. If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def toList(): List[A] = {
    foldRight[List[A]](Nil)((a,b) => a::b)
    //case Cons(h,t) => h() :: t().toListRecursive
    //case _ => List()
  }

  def take(n: Int): Stream[A] = {
    this match {
      case Cons(h, t) if (n>1) => cons(h(), t().take(n-1))
      case Cons(h, t) if (n==1) => cons(h(), empty)
      case _ => empty
    }
  }

  def drop(n: Int): Stream[A] = {
    this match {
      case Cons(h, t) if (n>0) => t().drop(n-1)
      case _ => this
    }
  }

  def takeWhile(p: A => Boolean): Stream[A] = {
    this match {
      case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
      case _ => empty
    }
  }

  def forAll(p: A => Boolean): Boolean = {
    this.foldRight(true)((a,b) => p(a) && b)
  }

  def takeWhileWithFold(p: A => Boolean): Stream[A] = {
    this.foldRight[Stream[A]](empty)((a,b) => if(p(a)) cons(a, b) else empty)
  }

  def headOption: Option[A] = {
    this.foldRight(None: Option[A])((a,b) => Some(a))
  }

  // 5.7 map, filter, append, flatmap using foldRight. Part of the exercise is
  // writing your own function signatures.

  def map[B](f: A => B): Stream[B] = {
    this.foldRight(empty: Stream[B])((a,b) => cons(f(a),b))
  }

  def filter(f: A => Boolean): Stream[A] = {
    foldRight(empty[A]) {
      ((a,b) => if(f(a)) cons(a, b) else b)
    }
  }

  def append[B >: A](stream2: => Stream[B]): Stream[B] = {
    foldRight(stream2)((a,b) => cons(a, b))
  }

  def flatMap[B](f: A => Stream[B]): Stream[B] = {
    this.foldRight(empty: Stream[B])((a,b) => f(a).foldRight(b)((x,b) => cons(x, b)))
  }



  def startsWith[B](s: Stream[B]): Boolean = ???
}
case object Empty extends Stream[Nothing]
case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty 
    else cons(as.head, apply(as.tail: _*))

  val ones: Stream[Int] = Stream.cons(1, ones)

  def constant(n: Int): Stream[Int] = {
    Stream.cons(n, constant(n))
  }

  def constantBetterPerformance(n: Int): Stream[Int] = {
    lazy val tail: Stream[Int] = Cons(() => n, () => tail)
    tail
  }

  def from(n: Int): Stream[Int] = {
    Stream.cons(n, from(n+1))
  }

  def fibs: Stream[Int] = {
    def loop(f0: Int, f1: Int): Stream[Int] = {
      cons(f0, loop(f1, f0+f1))
    }
    loop(0,1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] = {
    f(z) match {
      case None => empty
      case Some((a, s)) => cons(a, unfold(s)(f))
    }
  }

  def onesUnfold: Stream[Int] = unfold(1)(one => Some(one,one))
  def constantUnfold(n: Int): Stream[Int] = unfold(n)(constant => Some(constant,constant))
  def fromUnfold(n: Int): Stream[Int] = unfold(n)(from => Some(from,from+1))
  def fibsUnfold: Stream[Int] = {
    cons(0, unfold((0,1))(couple => Some((couple._2, (couple._2, couple._1+couple._2)))))
  }
}