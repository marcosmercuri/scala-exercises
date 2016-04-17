package com.book.chapter2

object Exercise5 extends App {

  def uncurry[A,B,C](f: A => (B => C)): (A, B) => C ={
    (a: A, b:B) => f(a)(b)
  }
}
