package lectures.Part3FP

object MapFlatMapFilterFor extends App {
  val list = List(1, 2, 3)
  println(list)

  println(list.map(_ + 1))
  println(list.map(_ + " is a number"))

  println(list.filter(_ % 2 == 0))
  val toPair = (x: Int) => List(x, x + 1)
  println(list.flatMap(toPair))

  val numbers = List(1, 2, 3, 4)
  val chars = List('a', 'b', 'c', 'd')
  val colors = List("black", "white")

  // iterations is done like flatMap...last one is map
  val combinations = numbers.flatMap(x => chars.flatMap(y => colors.map(z => s"$y$x-$z")))
  println(combinations)

  // foreach

  list.foreach(println)

  // for-comrehensions

  val forCombinations = for {
    n <- numbers if n % 2 == 0
    c <- chars
    color <- colors
  } yield s"$n$c-$color"

  for {
    n <- numbers
  } println(n)

  println(forCombinations)

  list.map { x=>
    x * 2
  }

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
    def foreach(f: A => Unit): Unit
    def sort(f: (A, A) => Int): GenMyList[A]
    def zipWith[B, C](list: GenMyList[B], zip: (A,B) => C): GenMyList[C]
    def fold[B](start: B)(operator: (B, A) => B): B
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
    def foreach(f: Nothing => Unit): Unit = ()
    def sort(f: (Nothing, Nothing) => Int): GenMyList[Nothing] = Empty
    def zipWith[B, C](list: GenMyList[B], zip: (Nothing,B) => C): GenMyList[C] =
      if(!list.isEmpty) throw new RuntimeException("Lists must have same lenght")
      else Empty
    def fold[B](start: B)(operator: (B, Nothing) => B): B = start
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
      if (predicate(h)) Cons(h, t.filter(predicate))
      else t.filter(predicate)
    def ++[B >: A](list: GenMyList[B]): GenMyList[B] =
      Cons(h, t ++ list)
    def foreach(f: A => Unit): Unit = {
      f(h)
      t.foreach(f)
    }
    def sort(f: (A, A) => Int): GenMyList[A] = {
      def insert(x: A, sortedList: GenMyList[A]): GenMyList[A] = {
        if (sortedList.isEmpty) new Cons(x, Empty)
        else if (f(x, sortedList.head)<= 0) Cons(x, sortedList)
        else Cons(sortedList.head, insert(x, sortedList.tail))
      }
      val sortedTail = t.sort(f)
      insert(h, sortedTail)
    }
    def zipWith[B, C](list: GenMyList[B], zip: (A,B) => C): GenMyList[C] =
      if(list.isEmpty) throw new RuntimeException("Lists must be the same length")
      else new Cons(zip(h, list.head), t.zipWith(list.tail, zip))
    def fold[B](start: B)(operator: (B, A) => B): B = t.fold(operator(start, h))(operator)
  }

  def toCurry(f: (Int, Int) => Int): (Int => Int => Int) =
    x => y => f(x,y)

  def fromCurry(f: (Int => Int => Int)): (Int, Int) => Int =
    (x, y) => f(x)(y)

  def compose[A, B, C](f: A => B, g: C => A): C => B =
    x => f(g(x))

  def andThen[A, B, C](f: A => B, g: B => C): A => C =
    x => g(f(x))

  def supperAdded2: (Int => Int => Int) = toCurry(_ + _)
  def simplerAdder = fromCurry(supperAdded2)
  def add4 = supperAdded2(4)
  def add2 = (x: Int) => x + 2
  def times3 = (x: Int) => x * 3

  val listOfInts: GenMyList[Int] = new Cons(1, new Cons(2, new Cons(3, Empty)))
  val cloneListOfInts: GenMyList[Int] = new Cons(1, new Cons(2, new Cons(3, Empty)))
  val listOfIntsAnother: GenMyList[Int] = new Cons(4, new Cons(5, Empty))
  println(listOfInts.map(_ * 2).toString)
  println(listOfInts.filter(_ % 2 == 0))
  println(listOfInts.flatMap(elem =>  new Cons(elem, new Cons(elem + 1, Empty))))
  println(listOfInts == cloneListOfInts)

  listOfInts.foreach(x => println(x))
  println(listOfInts.sort((x, y) => x - y))
  println(listOfInts.zipWith[Int, Int](cloneListOfInts, _ + _))
  println(listOfInts.fold(0)( _ + _))

  println(add4(17))
  println(simplerAdder(4, 17))

  val composed = compose(add2, times3)
  val ordered = andThen(add2, times3)

  println(composed(4))
  println(ordered(4))

  val combinationsFor = for {
    n <- listOfInts
    m <- listOfIntsAnother
  } yield s"$n $m"

  println(combinationsFor)

  abstract class Maybe[+T] {
    def map[B](f: T => B): Maybe[B]
    def flatMap[B](f: T => Maybe[B]): Maybe[B]
    def filter(p: T => Boolean): Maybe[T]
  }

  case object MaybeNot extends Maybe[Nothing] {
    def map[B](f: Nothing => B): Maybe[B] = MaybeNot
    def flatMap[B](f: Nothing => Maybe[B]): Maybe[B] = MaybeNot
    def filter(p: Nothing => Boolean): Maybe[Nothing] = MaybeNot
  }

  case class Just[+T](value: T) extends Maybe[T] {
    def map[B](f: T => B): Maybe[B] = Just(f(value))
    def flatMap[B](f: T => Maybe[B]): Maybe[B] = f(value)
    def filter(p: T => Boolean): Maybe[T] =
      if (p(value)) this
      else MaybeNot
  }

  val just3 = Just(3)
  println(just3)
  println(just3.map(_ * 2))
  println(just3.flatMap(x => Just(x % 2 == 0)))
  println(just3.filter(_ % 2 == 0))
}
