package AdvanceLectures.part3cc

import scala.concurrent.{Await, Future, Promise}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.{Failure, Random, Success, Try}

object FutureAndPromises extends App {
  def calculateMeaningOfLife: Int = {
    Thread.sleep(2000)
    42
  }

  val aFuture = Future {
    calculateMeaningOfLife
  }

  aFuture.onComplete {
    case Success(value) => println(value)
    case Failure(exception) => println(exception)
  } // onComplete called by some thread

  Thread.sleep(3000)
  case class Profile(id: String, name: String) {
    def poke(anotherProfile: Profile) =
      println(s"This $name poking ${anotherProfile.name}")
  }

  object SocialNetwork {
    val names = Map(
      "fb.id.1-zuck" -> "Mark",
      "fb.id.2-bill" -> "Bill",
      "fb.id.3-dummy" -> "Dummy"
    )

    val friends = Map(
      "fb.id.1-zuck" -> "fb.id.2-bill"
    )

    val random = new Random()

    def fetchProfile(id: String): Future[Profile] = Future {
      Thread.sleep(random.nextInt(300))
      Profile(id, names(id))
    }

    def fetchBestFriend(profile: Profile): Future[Profile] = Future {
      Thread.sleep(random.nextInt(400))
      val bfId = friends(profile.id)
      Profile(bfId, names(bfId))
    }
  }


  for {
    mark <- SocialNetwork.fetchProfile("fb.id.1-zuck")
    bill <- SocialNetwork.fetchBestFriend(mark)
  } yield {
    mark.poke(bill)
  }

  Thread.sleep(1000)

  case class User(name: String)
  case class Transaction(sender: String, receiver: String, amount: Double, status: String)

  object BankingApp {
    val name = "Rock the JVM banking"

    def fetchUser(name: String): Future[User] = Future {
      Thread.sleep(500)
      User(name)
    }

    def createTransaction(user: User, merchantName: String, amount: Double): Future[Transaction] = Future {
      Thread.sleep(1000)
      Transaction(user.name, merchantName, amount, "SUCCESS")
    }

    def purchase(username: String, item: String, merchantName: String, cost: Double): String = {
      val transactionStatusFuture = for {
        user <- fetchUser(username)
        transaction <- createTransaction(user, merchantName, cost)
      } yield transaction.status

      Await.result(transactionStatusFuture, 2.seconds)
    }
  }

  println(BankingApp.purchase("Daniel", "iPhone 12", "rock the jvm store", 3000))

  val promise = Promise[Int]() // controller over a future
  val future = promise.future

  //consumer
  future.onComplete {
    case Success(r) => println(s"consumer Ive received $r")
  }

  //Producer
  val producer = new Thread(() => {
    println("producer cruching")
    Thread.sleep(500)
    promise.success(42)
    println("producer done")
  })

  producer.start()
  Thread.sleep(1000)

  def fulfillImmediately[T](value: T): Future[T] = Future(value)

  def inSequence[A, B](first: Future[A], second: Future[B]): Future[B] = {
    first.flatMap(_ => second)
  }

  def first[A](fa: Future[A], fb: Future[A]): Future[A] = {
    val promise = Promise[A]
    fa.onComplete(promise.tryComplete)
    fb.onComplete(promise.tryComplete)
    promise.future
  }

  def last[A](fa: Future[A], fb: Future[A]): Future[A] = {
    val bothPromise = Promise[A]
    val lastPromise = Promise[A]
    val checkAndComplete = (result: Try[A]) =>
      if(!bothPromise.tryComplete(result))
        lastPromise.complete(result)

    fa.onComplete(checkAndComplete)
    fb.onComplete(checkAndComplete)
    lastPromise.future
  }

  val fast = Future {
    Thread.sleep(100)
    42
  }

  val slow = Future {
    Thread.sleep(200)
    45
  }

  first(fast, slow).foreach(println)
  last(fast, slow).foreach(println)

  Thread.sleep(500)

  def retryUntil[A](action: () => Future[A], condition: A => Boolean): Future[A] =
    action().filter(condition).recoverWith {
      case _ => retryUntil(action, condition)
    }

  val random = new Random()
  val action = () => Future {
    Thread.sleep(100)
    val nextValue = random.nextInt(100)
    println(s"generated $nextValue")
    nextValue
  }

  retryUntil(action, (x: Int) => x < 50).foreach(println)
  Thread.sleep(10000)
}
