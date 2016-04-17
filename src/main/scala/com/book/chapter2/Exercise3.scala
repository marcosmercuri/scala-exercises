package com.book.chapter2

object Exercise3 extends App {
  def partial1[A,B,C](a:A, f:(A,B)=>C): B => C = {
    b => f(a, b)
  }

  def app(x:Int, f:Int => Int): Int = {
      f(x+5)
  }

  val f = partial1(1, (x:Int,y:Int)=>x+y) // b => 1+b

  println(app(3, f))
}
