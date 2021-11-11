package lectures.Part3FP

object WhatIsFunction extends App {
  // use functions as first class elements

  trait MyFunction[A, B] {
    def apply(element: A): B
  }

  val doubler = new MyFunction[Int, Int] {
    override def apply(element: Int): Int = element * 2
  }

  println(doubler(2))

  // function type = Function1[A,B] support up to 22 parameters
  val stringToIntConvert = new Function1[String, Int] {
    override def apply(string: String): Int = string.toInt
  }

  println(stringToIntConvert("3") + 4)

  val adder: ((Int, Int) => Int) = new Function2[Int, Int, Int] {
    override def apply(a: Int, b: Int): Int = a + b
  }

  //function types Function2[A, B, R] === (A,B) => R
  //All scala functions are objects

  def concat: (String, String) => String = new Function2[String, String, String] {
    override def apply(a: String, b: String): String = a + b
  }

  println(concat("a", "b"))

  abstract class GenMyList[+A] {
    def head: A
    def tail: GenMyList[A]
    def isEmpty: Boolean
    def add[B >: A](element: B): GenMyList[B]
    override def toString: String = s"[ $printElements ]"
    def printElements: String
    def map[B](transform: A => B): GenMyList[B]
    def flatMap[B](transform: A => GenMyList[B]): GenMyList[B]
    def filter(predicate: A => Boolean): GenMyList[A]
    def ++[B >: A](list: GenMyList[B]): GenMyList[B]
  }

  case object Empty extends GenMyList[Nothing] {
    def head: Nothing = throw new NoSuchElementException
    def tail: GenMyList[Nothing] = throw new NoSuchElementException
    def isEmpty: Boolean = true
    def add[B >: Nothing](element: B): GenMyList[B] = new Cons(element, Empty)
    def printElements: String = ""
    def map[B](transform: Nothing => B): GenMyList[B] = Empty
    def flatMap[B](transform: Nothing => GenMyList[B]): GenMyList[B] = Empty
    def filter(predicate: Nothing => Boolean): GenMyList[Nothing] = Empty
    def ++[B >: Nothing](list: GenMyList[B]): GenMyList[B] = list
  }

  case class Cons[+A](h: A, t: GenMyList[A]) extends GenMyList[A] {
    def head: A = h
    def tail: GenMyList[A] = t
    def isEmpty: Boolean = false
    def add[B >: A](element: B): GenMyList[B] = new Cons(element, this)
    def printElements: String = if (t.isEmpty) h.toString else s"$h ${t.printElements}"
    def map[B](transform: A => B): GenMyList[B] =
      new Cons(transform(h), t.map(transform))
    def flatMap[B](transform: A => GenMyList[B]): GenMyList[B] = transform(h) ++ t.flatMap(transform)
    def filter(predicate: A => Boolean): GenMyList[A] =
      if (predicate(h)) new Cons(h, t.filter(predicate))
      else t.filter(predicate)
    def ++[B >: A](list: GenMyList[B]): GenMyList[B] =
      new Cons(h, t ++ list)
  }

  val listOfInts: GenMyList[Int] = new Cons(1, new Cons(2, new Cons(3, Empty)))
  val cloneListOfInts: GenMyList[Int] = new Cons(1, new Cons(2, new Cons(3, Empty)))
  val listOfIntsAnother: GenMyList[Int] = new Cons(4, new Cons(5, Empty))
  println(listOfInts.map(new Function1[Int, Int] {
    override def apply(elem: Int): Int = elem * 2
  }).toString)

  println(listOfInts.filter(new Function1[Int, Boolean] {
    override def apply(element: Int): Boolean = element % 2 == 0
  }))

  println(listOfInts.flatMap(new Function1[Int, GenMyList[Int]] {
    override def apply(element: Int): GenMyList[Int] = new Cons(element, new Cons(element + 1, Empty))
  }))

  println(listOfInts == cloneListOfInts)

  val specialFunction: Function1[Int, Function1[Int, Int]] = new Function1[Int, Function1[Int, Int]] {
    override def apply(x: Int): Int => Int = new Function[Int, Int] {
      override def apply(y: Int): Int = x + y
    }
  }

  val test = specialFunction(3)
  println(test(4))
  println(specialFunction(3)(4))
}
