package lectures.Part2Op

object Objects extends App {
  object Person {
    val N_EYES = 2
    def canFly: Boolean = false
  }
  class Person {

  }

  println(Person.N_EYES)
  println(Person.canFly)
}
