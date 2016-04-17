package com.book.chapter2

object Exercise2 extends App {

  def isSorted[A](as: Array[A], f:(A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop (x:Int): Boolean = {
      if (x >= as.length-1) true
      else if (f(as(x), as(x+1))) loop(x+1)
      else false
    }
    loop(0)
  }

  println("Is the array [1, 3, 4, 5,8] is sorted from asc?, " + isSorted( Array(1, 3, 4, 5, 8) , (x:Int , y:Int) => x<=y))
  println("Is the array [1, 3, 10, 5,8] is sorted from asc?, " + isSorted( Array(1, 3, 10, 5, 8) , (x:Int , y:Int) => x<=y))
  println("Is the array [8, 7, 6, 5,2] is sorted from desc?, " + isSorted( Array(8, 7, 6, 5,2) , (x:Int , y:Int) => x>=y))
}
