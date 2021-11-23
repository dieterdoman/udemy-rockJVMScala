package AdvanceLectures.part1as

object AdvancedPatternMatching extends App {
  val numbers = List(1)
  numbers match {
    case head :: Nil => println(s"the only element is $head")
    case _ => println("other")
  }

  class Person(val name: String, val age: Int)

  object Person {
    def unapply(person: Person): Option[(String, Int)] = Some((person.name, person.age))

    def unapply(age: Int): Option[String] =
      Some(if (age < 21) "minor" else "major")
  }

  val bob = new Person("Bob", 21)
  val greetings = bob match {
    case Person(n, a) => s"$n is $a old"
  }

  println(greetings)

  val legalStatus = bob.age match {
    case Person(status) => s"My leagla status is $status"
  }

  println(legalStatus)

  val n: Int = 44

  val mathProperty = n match {
    case x if x < 10 => "single digit"
    case x if x % 2 == 0 => " an even number"
    case _ => "no property"
  }

  object even {
    def unapply(arg: Int): Option[Boolean] = if (arg % 2 == 0) Some(true) else None
  }

  object singleDigit {
    def unapply(arg: Int): Option[Boolean] = if (arg < 10 && arg > -10) Some(true) else None
  }

  val mathNeatProperty = n match {
    case even(_) => s"$n Even number"
    case singleDigit(_) => s"$n is single digit"
    case _ => "other number"
  }

  println(mathNeatProperty)

  case class Or[A, B](a: A, b: B)

  val either = Or(2, "two")

  val humanDescription = either match {
    case number Or string => s"$number is written as $string"
  }

  println(humanDescription)

  val vararg = numbers match {
    case List(1, _*) => "starting with 1"
  }

  abstract class Mylist[+A] {
    def head: A = ???
    def tail: Mylist[A] = ???
  }

  case object Empty extends Mylist[Nothing]
  case class Cons[+A](override val head: A, override val tail: Mylist[A]) extends Mylist[A]

  object Mylist {
    def unapplySeq[A](list: Mylist[A]): Option[Seq[A]] =
      if (list == Empty) Some(Seq.empty)
      else unapplySeq(list.tail).map(list.head +: _)
  }

  val mylist: Mylist[Int] = Cons(1, Cons(2, Cons(3, Empty)))
  val decomposed = mylist match {
    case Mylist(1, 2, _*) => "Starting with 1 and 2"
    case _ => "Something else"
  }

  println(decomposed)

  // custom return types for unapply
  // isEmpty: Boolean, get: something

  abstract class Wrapper[T] {
    def isEmpty: Boolean
    def get: T
  }

  object PersonWrapper {
    def unapply(person: Person): Wrapper[String] = new Wrapper[String] {
      override def isEmpty: Boolean = false
      override def get: String = person.name
    }
  }

  val bobMatch = bob match {
    case PersonWrapper(n) => s"This person's name is $n"
    case _ => "An alien"
  }

  println(bobMatch)
}
