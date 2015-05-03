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
			case Cons(x, xs) => f(x, foldRight(xs, z)(f))  //no tail recursion here!
	}

	def sum2(ns: List[Int]) =
			foldRight(ns, 0)((x,y) => x + y)

			def product2(ns: List[Double]) =
			foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

			/*
			 * Implementation notice:
			 * 
			 *  if one of the variables is not used in the pattern matching, please use the variable pattern _
			 */
			def tail[A](l: List[A]): List[A] = l match {
			case Nil         => sys.error("tail of empty list") //fix copied from authors solution
			case Cons(_, xs) => xs
	}


	def setHead[A](l: List[A], h: A): List[A] = l match {
	case Nil => Nil
	case Cons(_, xs) => Cons(h, xs)
	}

	//exercise 3.4
	def drop[A](l: List[A], n: Int): List[A] = {
			if(n<=0) l    //the check on n MUST be done before the pattern matching
			else l match {
			case Nil => sys.error("Impossible to remove elements from an empty list")
			case Cons(x,xs) => drop(xs,n-1) 
			}
	}

	//exercise 3.5
	/**
	 * Please, refer to the authors' implementation for a better version
	 */
	def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
	case Nil => Nil
	case Cons(x,xs) => if(f(x)) dropWhile(xs, f) else l
	}


//My approach was to use a reverse list. In this way no side effects have been introduced
	def init[A](l: List[A]): List[A] = {
  //helper function to aggregate the value of the current list in an aggregation one, where the last element is removed.
			def aggregateFunction(currentList: List[A], aggregateList: List[A]): List[A] = {
					currentList match {
            
					case Nil => aggregateList

					case Cons(h,t) => {
						t match {
						case Nil => aggregateList
						case Cons(th,tt) => aggregateFunction(t, Cons(h,aggregateList))
						}
					}
					}
			}
      //since the removal function returns a list in reversed order...
		val listWithRemoval = aggregateFunction(l, Nil)
      
    //I need to iterate again over the list returned from the aggregate function in order to obtain a proper result.
     def reverseList(listToReverse:List[A], resultList:List[A]): List[A] = listToReverse match {
          case Nil => resultList
          case Cons(h,t) => reverseList(t, Cons(h,resultList))
       }   
      
     reverseList(listWithRemoval, Nil)
	}

	def length[A](l: List[A]): Int = {
    foldRight(l, 0)((_,y) => y + 1)
  }
  
  @annotation.tailrec
	def foldLeft[A,B](l: List[A], z: B)(f: (B, A) => B): B = {
    l match {
      case Nil => z
      case Cons(x,xs) => foldLeft(xs, f(z,x))(f)
    } 
  }
  
  def sum3(l:List[Int]): Int = {
    foldLeft(l, 0)(_ + _)
  }
  
  def product3(l:List[Int]): Double = {
    foldLeft(l, 1D)(_ * _)
  }
  
  def length2[A](l:List[A]): Double = {
    foldLeft(l, 0)((accumulator,_) => accumulator + 1)
  }
  
  def reverse[A](l:List[A]): List[A] = {
    foldLeft(l, Nil:List[A])((x,y) => Cons(y,x))
  }

	def map[A,B](l: List[A])(f: A => B): List[B] = sys.error("todo")
}

object ListTester {
	import List._

	def main(args:Array[String]) {
		//exercise 3.1
		println(x==3)

		//exercise 3.2 small test
		val originalList = List(1,2,3,4,5)
		println(tail(originalList)toString)

		//exercise 3.3
		println(setHead(originalList,15)toString)
		println(setHead(Nil,18)toString)

		//exercise 3.4
		println(drop(originalList,2)toString)
		println(drop(originalList,0)toString)
		println(drop(originalList,4)toString)
		// println(drop(originalList,8)toString)

		//exercise 3.5
		val evenButOneList = List(4,6,8,1,10)
		println(dropWhile(evenButOneList,(x:Int) => x % 2 == 0)toString)

		//exercise 3.6
		val listWithLastToRemove = List(1,2,3,4,5,6,7,8)
		println(init(listWithLastToRemove)toString)
    
    //exercise 3.9
    println(length(listWithLastToRemove))
    println(length(List()))
    println(length(List(1)))
    
    //exercise 3.10 and 3.11
    val listToElaborate = List(1,2,3,4)
    println(sum3(listToElaborate) == 10)
    println(product3(listToElaborate) == 24)
    println(length(listToElaborate) == length2(listToElaborate))
    
    //exercise 3.12
    val reverseByFold = List(1,2,3)
    println(reverse(reverseByFold).toString)
    
	}
}
