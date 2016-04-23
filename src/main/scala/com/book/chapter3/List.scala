package com.book.chapter3

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

// Companion object
object List {

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  // Exercise 2
  def tail[A](list: List[A]): List[A] = list match {
    case Nil => sys.error("tail of empty list")
    case Cons(h, t) => t
  }

  // Exercise 3
  def drop[A](l: List[A], n:Int): List[A] = {
    def dropUntilN(lista: List[A], count:Int): List[A] = lista match {
        case Nil => Nil
        case Cons(_, t) => if (count==n) t else dropUntilN(t, n+1)
    }

    dropUntilN(l, 1)
  }

  // Exercise 4
  def dropWhile[A](l: List[A])(f: A => Boolean) : List[A] = {
    l match {
      case Nil => Nil
      case Cons(h, t) => if (f(h)) dropWhile(t)(f) else l
    }
  }

  // Exercise 5
  def setHead[A](l: List[A], newH:A): List[A] = l match {
      case Nil => Cons(newH, Nil)
      case Cons(_, t) => Cons(newH, t)
  }

  // Exercise 6
  def init[A](l: List[A]): List[A] =  l match {
    case Nil => sys.error("init of empty list")
    case Cons(_,Nil) => Nil
    case Cons(h,t) => Cons(h,init(t))
  }


  // Exercise 9
  def length[A](l: List[A]): Int = {
      foldRight(l, 0)((_,y)=> 1+y)
  }

  // Exercise 10
  def foldLeft[A, B](l: List[A], z:B)(f:(B,A) => B): B = {
    @annotation.tailrec
    def go(l: List[A], acc:B): B = l match {
      case Nil => acc
      case Cons(h, t) => go(t, f(acc, h))
    }

    go(l,z)
  }

  // Exercise 12
  /*def reverse[A](l: List[A]):List[A] = {
    foldLeft(l, List[A]())((acc, t)=>Cons(t, acc))
  }*/

  // Exercise 14
  def appendThoughFoldRight[A](a1: List[A], a2: List[A]): List[A] = {
    foldRight(a1, a2)((h, acc)=>Cons(h, acc))
  }

  // Exercise 15
  def concat[A](l:List[List[A]]):List[A] = {
    foldRight(l, Nil:List[A])(appendThoughFoldRight (_, _))
  }

  // Exercise 16
  def addOne(l:List[Int]): List[Int] = foldRight(l, Nil:List[Int])((h, acc) => Cons(h+1, acc))

  // Exercise 17
  def convertToString(l:List[Double]): List[String] = foldRight(l, Nil:List[String])((h, acc) => Cons(h.toString, acc))

  // Exercise 18
  def map[A,B](l: List[A])(f: A => B): List[B] = foldRight(l, Nil:List[B])((h, acc)=> Cons(f(h), acc))

  // Exercise 19
  def filter[A](l: List[A])(f: A => Boolean ): List[A] = foldRight(l, Nil:List[A])((h, acc) => if (f(h)) Cons(h, acc) else acc )

  // Exercise 20
  def flatMap[A,B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))

  // Exercise 22
  def addListsInt(l1: List[Int], l2:List[Int]): List[Int] = (l1, l2) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(h, t), Cons(h2, t2)) => Cons(h+h2, addListsInt(t, t2))
  }

  // Exercise 24
  def addLists[A, B, C](l1: List[A], l2:List[B])(f: (A,B) => C): List[C] = (l1, l2) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(h, t), Cons(h2, t2)) => Cons(f(h,h2), addLists(t, t2)(f))
  }

  // Exercise 24
  def hasSubsequence[A](l:List[A], sub:List[A]): Boolean = {
    @annotation.tailrec
    def loop(l: List[A], l2:List[A]): Boolean = (l, l2) match {
      case (Nil, Nil) => true
      case (_, Nil) => true
      case (Nil, _) => false
      case (Cons(h, t), Cons(h2, t2)) => if (h==h2) loop(t, t2) else false
    }

    l match {
      case Nil => false
      case Cons(_, t) => if (loop(l, sub)) true else hasSubsequence(t, sub)
    }
  }
}

object Exercise1 extends App {
  val x = Cons(1, Cons(4, Cons(7, Cons(8, Nil))))
  println("length: " + List.length(x))

  println("foldLeft: " + List.foldLeft(x, 0)(_+_))

  println("Add one: " + List.addOne(x))

  val sub = Cons(1, Cons(7, Nil))

  println("has sub: " + List.hasSubsequence(x, sub))
}

