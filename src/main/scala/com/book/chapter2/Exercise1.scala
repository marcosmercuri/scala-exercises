package com.book.chapter2

object Exercise1 extends App {

  def fibonacci(n: Int): Int = {
    def loop(n: Int, result: Int, prev: Int): Int = {
      if (n == 0)
        prev
      else
        loop (n - 1, prev + result, result)
    }
      loop(n, 1, 0)
  }

  println("Fibonnaci of 5 is " + fibonacci(0))
  println("Fibonnaci of 10 is " + fibonacci(1))

}
