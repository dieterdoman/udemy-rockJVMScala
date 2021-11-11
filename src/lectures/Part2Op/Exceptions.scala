package lectures.Part2Op

object Exceptions extends App{
  val x: String = null

  // val weirdValue = throw new NullPointerException

  // need to extend from throwable

  def getInt(withException: Boolean): Int = if (withException) throw new RuntimeException("Not int for you") else 42

  val potentialFail = try {
    getInt(true)
  } catch {
    case e: RuntimeException => println("caught")
  } finally {
    // code will get executed no matter what
    // is optional
    // only used for side effects
    println("finally")
  }

  println(potentialFail)

  class MyException extends Exception
  val exception = new MyException
  // throw exception
}
