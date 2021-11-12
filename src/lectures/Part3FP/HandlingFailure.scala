package lectures.Part3FP

import scala.util.{Failure, Random, Success, Try}

object HandlingFailure extends App {
  val aSuccess = Success
  val aFailure = Failure(new RuntimeException("SUPER"))

  println(aSuccess)
  println(aFailure)

  def unsafeMethod(): String = throw new RuntimeException("Not string for you")

  val potentialFailure = Try(unsafeMethod())
  println(potentialFailure)

  val anotherFailure = Try {
    throw new RuntimeException("FAIL")
  }

  println(potentialFailure.isSuccess)

  def backupMethod(): String = "VALID"

  val fallbackTry = Try(unsafeMethod()).orElse(Try(backupMethod()))
  println(fallbackTry)

  def betterUnsafeMethod(): Try[String] = Failure(new RuntimeException("FAIL"))
  def betterSafeMethod(): Try[String] = Success("Success")
  val betterFallback = betterUnsafeMethod() orElse betterSafeMethod()

  println(betterSafeMethod().map(_.toLowerCase))
  println(betterSafeMethod().flatMap(x => Success(x.toLowerCase)))
  println(betterSafeMethod().filter(_.contains(10)))

  val hostname = "localhost"
  val port = "8080"
  def renderHtml(page: String) = println(page)

  class Connection {
    def get(url: String): String = {
      val random = new Random(System.nanoTime())
      if (random.nextBoolean()) "<html>...</html>"
      else throw new RuntimeException("Connection failure")
    }
  }
  object HttpService {
    val random = new Random(System.nanoTime())

    def getConnect(host: String, port: String): Connection = {
      if (random.nextBoolean()) new Connection
      else throw new RuntimeException("Port stolen")
    }
  }

  val tryFor = for {
    connection <- Try(HttpService.getConnect(hostname, port))
    html <- Try(connection.get("url"))
  } yield renderHtml(html)
}
