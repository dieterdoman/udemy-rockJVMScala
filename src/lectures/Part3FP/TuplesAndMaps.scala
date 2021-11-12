package lectures.Part3FP

import scala.annotation.tailrec

object TuplesAndMaps extends App {
  val aTuple = (2, "Hello scala") // Tuple2[Int, String]
  println(aTuple._1)
  println(aTuple.copy(_2 = "goodbye java"))
  println(aTuple.swap)

  val aMap: Map[String, Int] = Map()
  val phonebook = Map(("dieter", 555555), "daniel" -> 12312313).withDefaultValue(-1)
  println(phonebook)
  println(phonebook.contains("Jim"))
  println(phonebook("dieter"))
  println(phonebook("Asdf"))

  val newPairing = "Mary" -> 1231
  val newPhonebook = phonebook + newPairing
  println(newPhonebook)
  println(phonebook.map(pair => pair._1.toLowerCase -> pair._2))

  println(phonebook.view.filterKeys(_.startsWith("di")))

  println(phonebook.view.mapValues(num => "000" + num))
  println(phonebook.toList)
  println(List("Daniel" -> 555).toMap)
  val names = List("Bob", "James", "dieter", "daniel")
  println(names.groupBy(name => name.charAt(0)))

  type Person = String
  type Friends = Set[String]
  type Network = Map[Person, Friends]
  def add(network: Network, person: Person): Network =
    network + (person -> Set())

  def friend(network: Network, a: Person, b: Person): Network = {
    val friendListOfA = network(a)
    val friendListOfB = network(b)
    network + (a -> (friendListOfA + b)) + (b -> (friendListOfB + a))
  }

  def unFriend(network: Network, a: Person, b: Person): Network = {
    val friendListOfA = network(a)
    val friendListOfB = network(b)
    network + (a -> (friendListOfA - b)) + (b -> (friendListOfB - a))
  }

  def remove(network: Network, person: Person): Network = {
    def removeAux(friends: Friends, networkAcc: Network): Network = {
      if (friends.isEmpty) networkAcc
      else removeAux(friends.tail, unFriend(networkAcc, person, friends.head))
    }
    val unfriended = removeAux(network(person), network)
    unfriended - person
  }

  val emptyNetwork: Network = Map()
  val network = add(add(emptyNetwork, "Bob"), "Mary")
  val friendedNetwork = friend(network, "Bob", "Mary")
  println(network)
  println(friendedNetwork)
  println(unFriend(friendedNetwork, "Bob", "Mary"))
  println(remove(friendedNetwork, "Bob"))

  val people = add(network, "Jim")
  val jimBob = friend(people, "Bob", "Jim")
  val testNet = friend(jimBob, "Bob", "Mary")
  println(testNet)

  def nFriends(network: Network, person: Person): Int = {
    if(!network.contains(person)) 0
    else network(person).size
  }
  println(nFriends(testNet, "Bob"))

  def mostFriends(network: Network): Person =
    network.maxBy(pair => pair._2.size)._1

  println(mostFriends(testNet))

  def nPeopleNotLifers(network: Network): Int =
    network.count(pair => pair._2.isEmpty)

  println(nPeopleNotLifers(testNet))

  def socialConnect(network: Network, a: Person, b: Person): Boolean = {
    @tailrec
    def bfs(target: Person, consideredPeople: Friends, discoveredPeople: Friends): Boolean = {
      if(discoveredPeople.isEmpty) false
      else {
        val person = discoveredPeople.head
        if (person == target) true
        else if (consideredPeople.contains(person)) bfs(target, consideredPeople, discoveredPeople.tail)
        else bfs(target, consideredPeople + person, discoveredPeople.tail ++ network(person))
      }
    }
    bfs(b, Set(), network(a) + a)
  }

  println(socialConnect(testNet, "Mary", "Jim"))
  println(socialConnect(network, "Mary", "Bob"))
}
