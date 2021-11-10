package lectures.Part1Basics

object Functions extends App {
  def afunction(a: String, b: Int): String = {
    a + " " + b
  }

  def repeatedFunction(a: String, n: Int): String = {
    if (n == 1)
      a
    else
      a + repeatedFunction(a, n - 1)
  }

  def greetingFunction(name: String, age: Int): String = {
    s"Hi my name is ${name} and my age is ${age}"
  }

  def factorial(n: Int): Int = {
    if (n <= 0)
      1
    else
      n * factorial(n - 1)
  }

  def fibonaci(n: Int): Int = {
    if (n <= 2)
      1
    else
      fibonaci(n - 1) + fibonaci(n - 2)
  }

  def isPrime(n: Int): Boolean = {
    def isPrimeUntil(t: Int): Boolean = {
      if (t <= 1)
        true
      else
      n % t != 0 && isPrimeUntil(t-1)
    }
    isPrimeUntil(n / 2)
  }
}
