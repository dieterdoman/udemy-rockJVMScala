package lectures.Part2Op

object AnonymousClasses extends App {

  abstract class Animal {
    def eat: Unit
  }

  val funnyAnimal: Animal = new Animal {
    override def eat: Unit = println("hahaha")
  }

  println(funnyAnimal.getClass)

  class Person(name: String) {
    def sayHi: Unit = println(s"Hi my name is $name")
  }

  val jim = new Person("jim") {
    override def sayHi: Unit = println("Jimmy")
  }
}
