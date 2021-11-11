package lectures.Part3FP

object AnonymousFunctions extends App {
  val doubler: Int => Int = (x: Int) => x * 2

  val adder: (Int, Int) => Int = (x: Int, b: Int) => x + b

  val justDoSomething: () => Int = () => 3

  println(justDoSomething)
  println(justDoSomething())

  val stringToInt = { (str: String) =>
    str.toInt
  }

  // need to specify the type
  val niceIncrementer: Int => Int = _ + 1
  val niceAdder: (Int, Int) => Int = _ + _

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
  println(listOfInts.map(_ * 2).toString)
  println(listOfInts.filter(_ % 2 == 0))
  println(listOfInts.flatMap(elem =>  new Cons(elem, new Cons(elem + 1, Empty))))
  println(listOfInts == cloneListOfInts)

  // (Int => (Int => Int))
  val specialFunction = (x: Int) => (y: Int) => x + y
  val test = specialFunction(3)
  println(test(4))
  println(specialFunction(3)(4))
}
