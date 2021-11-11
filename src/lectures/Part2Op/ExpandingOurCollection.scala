package lectures.Part2Op

object ExpandingOurCollection extends App {
  trait MyPredicate[-T] {
    def test(element: T): Boolean
  }

  trait MyTransform[-A, B] {
    def transform(element: A): B
  }

  abstract class GenMyList[+A] {
    def head: A
    def tail: GenMyList[A]
    def isEmpty: Boolean
    def add[B >: A](element: B): GenMyList[B]
    override def toString: String = s"[ $printElements ]"
    def printElements: String
    def map[B](transform: MyTransform[A, B]): GenMyList[B]
    def flatMap[B](transform: MyTransform[A, GenMyList[B]]): GenMyList[B]
    def filter(predicate: MyPredicate[A]): GenMyList[A]
    def ++[B >: A](list: GenMyList[B]): GenMyList[B]
  }

  object Empty extends GenMyList[Nothing] {
    def head: Nothing = throw new NoSuchElementException
    def tail: GenMyList[Nothing] = throw new NoSuchElementException
    def isEmpty: Boolean = true
    def add[B >: Nothing](element: B): GenMyList[B] = new Cons(element, Empty)
    def printElements: String = ""
    def map[B](transform: MyTransform[Nothing, B]): GenMyList[B] = Empty
    def flatMap[B](transform: MyTransform[Nothing, GenMyList[B]]): GenMyList[B] = Empty
    def filter(predicate: MyPredicate[Nothing]): GenMyList[Nothing] = Empty
    def ++[B >: Nothing](list: GenMyList[B]): GenMyList[B] = list
  }

  class Cons[+A](h: A, t: GenMyList[A]) extends GenMyList[A] {
    def head: A = h
    def tail: GenMyList[A] = t
    def isEmpty: Boolean = false
    def add[B >: A](element: B): GenMyList[B] = new Cons(element, this)
    def printElements: String = if (t.isEmpty) h.toString else s"$h ${t.printElements}"
    def map[B](transform: MyTransform[A, B]): GenMyList[B] =
      new Cons(transform.transform(h), t.map(transform))
    def flatMap[B](transform: MyTransform[A, GenMyList[B]]): GenMyList[B] = transform.transform(h) ++ t.flatMap(transform)
    def filter(predicate: MyPredicate[A]): GenMyList[A] =
      if (predicate.test(h)) new Cons(h, t.filter(predicate))
      else t.filter(predicate)
    def ++[B >: A](list: GenMyList[B]): GenMyList[B] =
      new Cons(h, t ++ list)
  }

  val listOfInts: GenMyList[Int] = new Cons(1, new Cons(2, new Cons(3, Empty)))
  val listOfIntsAnother: GenMyList[Int] = new Cons(4, new Cons(5, Empty))
  println(listOfInts.map(new MyTransform[Int, Int] {
    override def transform(elem: Int): Int = elem * 2
  }).toString)

  println(listOfInts.filter(new MyPredicate[Int] {
    override def test(element: Int): Boolean = element % 2 == 0
  }))

  println(listOfInts.flatMap(new MyTransform[Int, GenMyList[Int]] {
    override def transform(element: Int): GenMyList[Int] = new Cons(element, new Cons(element + 1, Empty))
  }))
}
