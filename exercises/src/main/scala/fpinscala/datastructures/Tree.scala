package fpinscala.datastructures

import java.util.concurrent.atomic.DoubleAccumulator

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

    def size[A](t: Tree[A]): Int = {
      t match {
        case Leaf(_) => 1
        case Branch(left, right) => 1 + size(left) + size(right)
      }
    }

    def maximum(t: Tree[Int]): Int = {
        t match {
          case Leaf(elem) => elem
          case Branch(left, right) => Math.max(maximum(left), maximum(right))
        }
    }

  def depth[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 0
      case Branch(left, right) => 1 + depth(left) max depth(right)
    }
  }

  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = {
    t match {
      case Leaf(a) => Leaf(f(a))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }
  }

  def fold[A,B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = {
    t match {
      case Leaf(a) => f(a)
      case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
    }
  }

  def sizeWithFold[A](t: Tree[A]): Int = {
    fold(t)(_ => 1)(1 + _ + _)
  }

  def depthWithFold[A](t: Tree[A]): Int = {
    fold(t)(_ => 0)((a,b) => 1 + a max b)
  }

  def maximumWithFold(t: Tree[Int]): Int = {
    fold(t)(elem => elem)((a,b) => a.max(b))
  }

  def mapWithFold[A,B](t: Tree[A])(f: A => B): Tree[B] = {
    fold(t)(elem => Leaf(f(elem)): Tree[B])((a,b) => Branch(a, b))
  }

}