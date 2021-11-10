package lectures.Part1Basics

import scala.annotation.tailrec

object Recursion extends App {
  def factorial(n: Int): Int = {
    if (n <= 0)
      1
    else {
      println(s"computing factorial of $n - 1 first need factorial of ${n - 1}")
      val result = n * factorial(n - 1)
      println(s"Computed factorial of $n")
      result
    }
  }

  def anotherFactorial(n: BigInt): BigInt = {
    @tailrec
    def factHelper(x: BigInt, accumalator: BigInt): BigInt = {
      if (x <= 1)
        accumalator
      else
        factHelper(x - 1, x * accumalator)
    }
    factHelper(n,1)
  }

  @tailrec
  def concatinateStringsNtimes(n: Int, string: String, accum: String): String = {
      if (n <= 1)
        accum
      else
        concatinateStringsNtimes(n - 1, string, s"${string} ${accum}")
  }

  def isPrime(n: Int): Boolean = {
    @tailrec
    def isPrimeUntil(t: Int, accum: Boolean): Boolean = {
      if (!accum) false
      else if (t <= 1)
        true
      else {
        isPrimeUntil(t - 1, n % t != 0 && accum)
      }
    }
    isPrimeUntil(n / 2, accum=true)
  }

  def fibonaci(n: Int): Int = {
    @tailrec
    def helper(x: Int, last: Int, nextToLast: Int): Int = {
      if (x >= n)
        last
      else
        helper(x + 1, last + nextToLast, last)
    }
    if (n <= 2) 1
    else helper(2, 1, 1)
  }
}
