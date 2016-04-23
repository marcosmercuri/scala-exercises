package com.book.chapter3

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  // Exercise 25
  def size[A](t:Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l,r) => 1 + size(l) + size(r)
  }

  // Exercise 26
  def maximum(t:Tree[Int]):Int = {
    def loop(t:Tree[Int], m:Int): Int = t match {
      case Leaf (x) => x max m
      case Branch(x,y) => loop(x, m) max loop(y, m)
    }
    loop(t,Int.MinValue)
  }

  // Exercise 27
  def depth[A](t:Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  // Exercise 28
  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(x) => Leaf(f(x))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  // Exercise 29
  def fold[A,B](t: Tree[A])(f: A => B)(g: (B,B) => B): B = t match {
    case Leaf(a) => f(a)
    case Branch(l,r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }
}
