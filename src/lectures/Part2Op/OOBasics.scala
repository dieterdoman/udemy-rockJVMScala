package lectures.Part2Op

object OOBasics extends App {
  val person = new Person("John", 26)
  println(person.age)
  person.greet("Daniel")
  person.greet()
}

class Person(name: String, val age: Int) {
  val x = 2

  println(1 + 3)

  def greet(name: String): Unit = {
    println(s"$name says ${this.name}")
  }

  def greet(): Unit = println(s"$name")

  def this(name: String) = this(name, 0)
}

class Writer(firstname: String, surname: String, val year: Int) {
  def fullname(): String = s"$firstname $surname"
}

class Novel(name: String, yearOfRelease: Int, author: Writer) {
  def authorAge(): Int = {
    yearOfRelease - author.year
  }

  def isWrittenBy(author: Writer): Boolean = {
    author == this.author
  }

  def copy(newYearOfRelease: Int): Novel = {
    new Novel(name, newYearOfRelease, author)
  }
}

class Counter(val count: Int) {
  def increment(): Counter = {
    new Counter(count+1)
  }

  def decrement(): Counter = {
    new Counter(count-1)
  }

  def increment(x: Int): Counter = {
    new Counter(count + x)
  }

  def decrement(x: Int): Counter = {
    new Counter(count - x)
  }
}