package AdvanceLectures.part2afp

object LazyEvaluation extends App {
  // lazy only evaluated only by need bases
  lazy val x: Int = {
    println("Hello")
    42
  }
  println(x)
  println(x)

  def sideEffectCondition: Boolean = {
    println("Boo")
    true
  }

  def simpleCondition: Boolean = false

  lazy val lazyCondition= sideEffectCondition
  println(if (simpleCondition && lazyCondition) "yes" else "no")

  def byNameMethod(n: => Int): Int = {
    lazy val t = n
    t + t + t + 1
  }
  def retrieveMagicValue = {
    Thread.sleep(1000)
    42
  }

  println(byNameMethod(retrieveMagicValue))

  //call by need for lazy

  def lessThan30(i: Int): Boolean = {
    println(s"$i less than 30?")
    i < 30
  }

  def greaterThan20(i: Int): Boolean = {
    println(s"$i greater than 20?")
    i > 20
  }

  val numbers = List(1, 25, 40, 5, 23)
  val lt30 = numbers.filter(lessThan30)
  val gr20 = numbers.filter(greaterThan20)
  println(gr20)

  val lt30Lazy = numbers.withFilter(lessThan30)
  val gt20Lazy = lt30Lazy.withFilter(greaterThan20)
  println
  gt20Lazy.foreach(println)

  // for-comprehensions use withFilter with guards

  for {
    a <- List(1,2,3) if x % 2 == 0
  } yield a + 1
  List(1,2,3).withFilter(_ % 2 == 0).map(_ + 1)
}
