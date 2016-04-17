package com.book.chapter2

object Exercise6 extends App {

  def compose[A,B,C](f:B=>C, g:A=>B): A=>C = {
    a:A => f(g(a))
  }

}
