package AdvanceLectures.part2afp

object CurriesPAF extends App {
  val supperAdder: Int => Int => Int = x => y => x + y

  val add3 = supperAdder(3)
  println(add3(5))
  println(supperAdder(3)(5))

  def curriedAdded(x: Int)(y: Int): Int = x + y
  val add4: Int => Int = curriedAdded(4)

  def inc(x: Int) = x + 1
  List(1,2,3).map(inc) // compilar .map(x => inc(x))

  // Partial function applications
  val add5 = curriedAdded(5) _ // Int => Int

  val simpleAddFunction = (x: Int, y: Int) => x + y
  def simpleAddMethod(x: Int, y: Int) = x + y
  def curriedAddMethod(x: Int)(y: Int) = x + y

  val add7 = (x: Int) => simpleAddFunction(7, x)
  val add7_2 = simpleAddFunction.curried(7)
  val add7_3 = curriedAddMethod(7) _
  val add7_4 = curriedAddMethod(7)(_)
  val add7_5 = simpleAddMethod(7, _: Int)
  val add7_6 = simpleAddFunction(7, _: Int)

  def concatenator(a: String, b: String, c: String) = a + b + c
  val insertName = concatenator("Hello ", _: String, " How are you?")
  println(insertName("Dieter"))
  val fillIntTheBlanks = concatenator("hello ", _: String, _: String)
  println(fillIntTheBlanks("Daniel", " Scala is awesome"))

  def format(string: String)(number: Double): String = string.format(number)
  val format2 = format("%4.2f") _
  println(format2(Math.PI))
  val format3 = format("%8.6f") _
  println(format3(Math.PI))

  def byName(n: => Int) = n + 1
  def byFunction(f: () => Int) = f() + 1
  def method: Int = 42
  def parenMethod(): Int = 42

  byName(23)
  byName(method)
  byName(parenMethod()) // be careful for byName and byFunction parameters
  byName((() => 42)())

  byFunction(parenMethod)
  byFunction(() => 42)
}
