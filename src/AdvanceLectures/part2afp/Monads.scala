package AdvanceLectures.part2afp

object Monads extends App {
  trait Attempt[+A] {
    def flatMap[B](f: A => Attempt[B]): Attempt[B]
  }
  object Attempt {
    def apply[A](a: => A): Attempt[A] =
      try {
        Success(a)
      } catch {
        case e: Throwable => Fail(e)
      }
  }

  case class Success[+A](value: A) extends Attempt[A] {
    def flatMap[B](f: A => Attempt[B]): Attempt[B] = {
      try {
        f(value)
      } catch {
        case e: Throwable => Fail(e)
      }
    }
  }

  case class Fail(e: Throwable) extends Attempt[Nothing] {
    def flatMap[B](f: Nothing => Attempt[B]): Attempt[B] = this
  }

  //left-identity Attempt(x).flatmap(f) = f(x)
  //right-identity attempt.flatmap(unit) = attempt
  //associativity attempt.flatmap(f).flatmap(g) = attempt(x => f(x).flatmap(g))

  class Lazy[+A](value: => A) {
    private lazy val internalValue = value
    def use: A = internalValue
    def flatMap[B](f: (=> A) => Lazy[B]): Lazy[B] = f(internalValue)
  }
  object Lazy {
    def apply[A](value: => A): Lazy[A] = new Lazy(value)
  }

  val lazyInstance = Lazy {
    println("I am the lazy one")
    42
  }

  val flatMappedInstance = lazyInstance.flatMap(x => Lazy(10 * x))
  val flatMappedInstance2 = lazyInstance.flatMap(x => Lazy(10 * x))
  println(flatMappedInstance.use)
  println(flatMappedInstance2.use)
}
