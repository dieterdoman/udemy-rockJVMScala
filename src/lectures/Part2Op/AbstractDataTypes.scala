package lectures.Part2Op

object AbstractDataTypes extends App {
  abstract class Animal {
    val creatureType: String = "Wild"
    def eat: Unit
  }

  class Dog extends Animal {
    override val creatureType: String = "K9"
    def eat: Unit = println("crunch")
  }

  trait Carnivore {
    def eat(animal: Animal): Unit
    val preferredMeal: String = "fresh meat"
  }

  class Crocodile extends Animal with Carnivore {
    override val creatureType: String = "Croc"
    def eat: Unit = "nomnom"
    def eat(animal: Animal): Unit = println(s"${creatureType} eating ${animal.creatureType}")
  }

  val dog = new Dog
  val croc = new Crocodile
  croc.eat(dog)

  //traits vs abstract classes
  // traits do not have constructor parameters
  // multiple traits can be multiple inheritances
  // traits behavior, abstract is type of thing
}
