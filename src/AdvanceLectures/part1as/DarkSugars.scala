package AdvanceLectures.part1as

import scala.util.Try

object DarkSugars extends App {
  def singleArgMethod(arg: Int): String = s"$arg little ducks..."

  val description = singleArgMethod {
    // write some code
    42
  }

  val aTryInstance = Try {
    throw new RuntimeException
  }

  List(1,2,3).map { x =>
    x + 1
  }

  trait Action {
    def act(x: Int): Int
  }

  val anInstance: Action = new Action {
    override def act(x: Int): Int = x + 1
  }

  val aFunkyInstance: Action = (x: Int) => x + 1

  // example: Runnables

  val aThread: Thread = new Thread(() => println("sweet"))

  abstract class AnAbstractType {
    def implemented: Int = 23
    def f(a: Int): Unit
  }

  val anAbstractInstance: AnAbstractType = (a: Int) => println("sweet")

  val prependedList = 2 :: List(3,4)

  // List(3,4).::(2)
  // scala spec: last char decided associativity of the method : means right, anything else is left

  1 :: 2 :: 3 :: List(4,5)
  List(4,5).::(3).::(2).::(1)

  class MyStream[T] {
    def -->:(value: T): MyStream[T] = this
  }

  val myStream = 1 -->: 2 -->: 3 -->: new MyStream[Int]

  class TeenGirl(name: String) {
    def `and then said`(gossip: String): Unit = println(s"$name said $gossip")
  }

  val lilly = new TeenGirl("Lilly")
  lilly `and then said` "Scala is so sweet"

  class Composite[A, B]
  val composite: Composite[Int, String] = new Composite[Int, String]

  val composite1: Int Composite String = new Composite[Int, String]

  class -->[A, B]
  val towards: Int --> String = new -->[Int, String]

  val anArray = Array(1,2,3)
  anArray(2) = 7
  // anArray.update(2, 7)

  class Mutable {
    private var internalMember: Int = 0
    def member = internalMember
    def member_=(value: Int): Unit = internalMember = value
  }

  val aMutableContainer = new Mutable
  aMutableContainer.member = 42
  //aMutableContainer.member_=(42)
}
