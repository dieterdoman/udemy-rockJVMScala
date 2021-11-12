package lectures.Part3FP

import scala.util.Random

object Sequences extends App {
  val aSequence = Seq(2, 1, 3, 4)
  println(aSequence)
  println(aSequence.reverse)
  println(aSequence(2))
  println(aSequence ++ Seq(6, 7, 5))
  println(aSequence.sorted)

  val aRange: Seq[Int] = 1 until 10
  println(aRange)

  (1 to 10).foreach(println(_))

  val aList = List(1,2,3)
  val prepend = 42 +: aList :+ 89
  println(prepend)

  val apples5 = List.fill(5)("apples")
  println(apples5)
  println(aList.mkString("-|-"))

  val numbers = Array(1,2,3,4)
  val threeElements = Array.ofDim[String](3)
  threeElements.foreach(println)

  //mutable
  numbers(2) = 0
  println(numbers.mkString((" ")))

  val numbersSeq: Seq[Int] = numbers

  val vector: Vector[Int] = Vector(1,2,3)
  println(vector)

  val maxRuns = 1000
  val maxCapacity = 1000000
  def getWriteTime(collect: Seq[Int]): Double = {
    val r = new Random()
    val times = for {
      it <- 1 to maxRuns
    } yield {
      val currentTime = System.nanoTime()
      collect.updated(r.nextInt(maxCapacity), r.nextInt())
      System.nanoTime() - currentTime
    }
    times.sum * 1.0 / maxRuns
  }

  val numbersList = (1 to maxCapacity).toList
  val numberVector = (1 to maxCapacity).toVector
  println(getWriteTime(numbersList))
  println(getWriteTime(numberVector))
}
