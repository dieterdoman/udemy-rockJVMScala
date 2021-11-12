package lectures.Part4PatternMatching

import scala.util.Random

object PatternMatching extends App {
  val random = new Random()
  val x = random.nextInt(10)

  val description = x match {
    case 1 => "the one"
    case 2 => "double our nothing"
    case _ => "Something else"
  }

  println(x)
  println(description)

  case class Person(name: String, age: Int)
  val bob = Person("Bob", 20)

  // cases are matched in order
  val greeting = bob match {
    case Person(n, a) if a < 21 => s"My name is $n age is $a, I am youngling"
    case Person(n, a) => s"My name is $n age is $a"
    case _ => "I dont know who I am"
  }
  println(greeting)

  // Gives warning if all cases are not used
  sealed class Animal
  case class Dog(breed: String) extends Animal
  case class Parrot(greeting: String) extends Animal

  val animal: Animal = Dog("Odi")
  animal match {
    case Dog(someBreed) => println(s"$someBreed")
    case Parrot(greeting) => println(s"$greeting")
  }

  trait Expr
  case class Number(n: Int) extends Expr
  case class Sum(e1: Expr, e2: Expr) extends Expr
  case class Prod(e1: Expr, e2: Expr) extends Expr

  def humanReadableForm(expr: Expr): String = {
    expr match {
      case Number(n) => s"$n"
      case Sum(e1, e2) => s"${humanReadableForm(e1)} + ${humanReadableForm(e2)}"
      case Prod(e1, e2) => {
        def maybeShowParent(e: Expr) = e match {
          case Prod(_, _) => humanReadableForm(e)
          case Number(_) => humanReadableForm(e)
          case _ => s"(${humanReadableForm(e)})"
        }
        s"${maybeShowParent(e1)} * ${maybeShowParent(e2)}"
      }
    }
  }

  println(humanReadableForm(Sum(Number(2), Number(3))))
  println(humanReadableForm(Prod(Sum(Number(2), Number(1)), Number(3))))
  println(humanReadableForm(Sum(Prod(Number(2), Number(1)), Number(3))))
}
