package lectures.Part2Op

object Inheritance extends App {
  class Animal {
    val creatureType = "Wild"
    def eat = println("nomnom")
  }

  class Cat extends Animal

  val cat = new Cat
  cat.eat

  class Person(name: String, age: Int) {
    def this(name:String) = this(name, 0)
  }
  class Adult(name: String, age: Int, idCard: String) extends Person(name)

  class Dog(override val creatureType: String) extends Animal {
    override def eat = {
      super.eat
      println("crunch")
    }
  }
  val dog = new Dog("Domestic")
  dog.eat

  val unknownAnimal: Animal = new Dog("asdf")
  unknownAnimal.eat

  // final key prevents over riding either on method, val or whole class.
  // seal the class, extends class in THIS file, prevent it in other files.
}
