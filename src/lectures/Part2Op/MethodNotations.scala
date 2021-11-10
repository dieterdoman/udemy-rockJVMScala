package lectures.Part2Op

import scala.language.postfixOps

object MethodNotations extends App {
  class Person(val name: String, favouriteMovie: String, val age: Int = 0) {
    def likes(movie: String): Boolean = movie == favouriteMovie
    def +(person: Person): String = s"${this.name} hangout with ${person.name}"
    def unary_! : String = s"$name, AAAAHHHHHH"
    def isAlive: Boolean = true
    def apply(): String = s"Hi my name is $name"
    def +(nickname: String): Person = new Person(s"${name} is ($nickname)", favouriteMovie)
    def unary_+ : Person = new Person(name, favouriteMovie, age + 1)
    def learns(item: String): String = s"$name learns $item"
    def learnsScala: String = this.learns("Scala")
    def apply(number: Int): String = s"$favouriteMovie * $number"
  }

  val mary = new Person("Mary", "Inception")
  println(mary.likes("Inception"))
  println(mary likes "Inception")

  val tom = new Person("Tom", "Fight Club")
  println(mary + tom)
  println(mary.+(tom))
  println(1.+(2))

  val x = -1
  val y = 1.unary_-

  println(!mary)
  println(mary.unary_!)

  println(mary.isAlive)
  println(mary isAlive)

  println(mary.apply())
  println(mary())

  println((mary + "the rockstar").apply())
  println((+mary).age)
  println(mary learnsScala)
  println(mary(2))
}
