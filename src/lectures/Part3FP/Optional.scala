package lectures.Part3FP

import scala.util.Random

object Optional extends App {
  val myFirstOption: Option[Int] = Some(1)
  val noOption: Option[Int] = None

  println(myFirstOption)

  def unsafeMethod(): String = null
  val result = Option(unsafeMethod())

  def backupMethond(): String = "valid result"

  val chainedResult = Option(unsafeMethod()).orElse(Option(backupMethond()))
  println(chainedResult)

  def betterUnsafeMethod: Option[String] = None
  def betterBackupMethod: Option[String] = Some("valid result")
  val betterChainedResult = betterUnsafeMethod orElse betterBackupMethod
  println(betterChainedResult)

  println(myFirstOption.isEmpty)
  println(myFirstOption.get) // UNSAFE

  println(myFirstOption.map(_ * 2))
  println(noOption.map(_ * 2))
  println(myFirstOption.filter(x => x > 10))
  println(myFirstOption.flatMap(x => Option(x * 10)))

  val config: Map[String, String] = Map("host" -> "127.0.0.1", "port" -> "80")
  class Connection {
    def connect = "Connected"
  }
  object Connect {
    val random = new Random(System.nanoTime())
    def apply(host: String, port: String): Option[Connection] = {
      if (random.nextBoolean()) Some(new Connection)
      else None
    }
  }

  val con = for {
    host <- config.get("host")
    port <- config.get("port")
    connection <- Connect(host, port)
  } yield connection.connect

  println(con)
}
