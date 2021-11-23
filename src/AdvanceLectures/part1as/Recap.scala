package AdvanceLectures.part1as

import scala.annotation.tailrec

object Recap extends App {
  val aCondition: Boolean = false
  val aConditioned = if (aCondition) 42 else 65

  val aCodeBlock = {
    if (aCondition) 54
    56
  }

  val theUnit = println("Hello scala")

  def aFunction(x: Int): Int = x + 1

  @tailrec
  def factorial(n: Int, accumulator: Int): Int = {
    if (n <= 0) accumulator
    else factorial(n - 1, n * accumulator)
  }

  class Animal
  class Dog extends Animal
  val aDog: Animal = new Dog

  trait Carnivore {
    def eat(a: Animal): Unit
  }

  class Crocodile extends Animal with Carnivore {
    def eat(a: Animal): Unit = println("crunch")
  }

  val aCroc = new Crocodile
  aCroc.eat(aDog)
  aCroc eat aDog

  val aCarnivore = new Carnivore {
    override def eat(a: Animal): Unit = println("meat eater crunch")
  }

  abstract class Mylist[+A]

  object Mylist

  case class Person(name: String, age: Int)

  val aPotentialFailure = try {
    throw new RuntimeException
  } catch {
    case e: Exception => "Caught exception"
  } finally {
    println("some logs")
  }

  val incrementer = new Function[Int, Int] {
    override def apply(v1: Int): Int = v1 + 1
  }

  incrementer(1)

  val anonymousIncrementer = (x: Int) => x + 1
  List(1,2,3).map(anonymousIncrementer)

  val pairs = for {
    num <- List(1,2,3)
    char <- List("a", "b", "c")
  } yield s"$num-$char"

  val aMap = Map("Daniel" -> 69, "Jess" -> 555)

  val anOption = Some(2)

  val x = 2
  val order = x match {
    case 1 => "First"
    case 2 => "Second"
    case _ => "Other"
  }

  val bob = Person("Bob", 22)
  val greeting = bob match {
    case Person(n, _) => s"Hi my name is $n"
  }
}
