package lectures.Part4PatternMatching

import lectures.Part3FP.HOfsCurries.{Cons, Empty, GenMyList}

object AllThePatterns extends App {
  val x: Any = "Scala"
  val constants = x match {
    case 1 => "A number"
    case "Scala" => "The scala"
    case true => "The Truth"
    case AllThePatterns => "Singleton object"
  }

  val matchAnything = x match {
    case _ => ""
  }

  val matchAVariable = x match {
    case something => s"something $something"
  }

  val aTuple = (1, 2)
  val matchATuple = aTuple match {
    case (1, 1) => ""
    case (something, 2) => s"something $something"
  }

  val nestedTuple = (1, (2, 3))
  val matchNestedTuple = nestedTuple match {
    case (_, (2, v)) => ""
  }

  // constructor pattern
  val aList: GenMyList[Int] = Cons(1, Cons(2, Empty))
  val matchAList = aList match {
    case Empty => "Empty"
    case Cons(head, Cons(subhead, subtail)) => s"$head, $subhead $subtail"
  }

  val aStandardList = List(1,2,3,42)
  val standardListMatch = aStandardList match {
    case List(1, _, _, _) => "Extractor"
    case List(1, _*) => "List of arbitrary length"
    case 1 :: List(_) => "infix pattern"
    case List(1,2,3) :+ 42 => "another infix pattern"
  }

  val unknown: Any = 2
  val unknownMatch = unknown match {
    case list: List[Int] => "its a list, explicit type specifie"
    case _ => ""
  }

  val nameBindingMatch = aList match {
    case nonEmptyList @ Cons(_, _) => s"$nonEmptyList"
    case Cons(1, rest @ Cons(2, _)) => s"$rest"
  }

//  val multiPattern = aList match {
//    case Empty | Cons(0, _) => "compound pattern (multi-pattern)"
//  }

  val secondElement = aList match {
    case Cons(_, Cons(specialElement, _)) if specialElement % 2 == 0 => "if guards"
  }

  val numbers = List(1, 2, 3)
  val numbersMatch = numbers match {
    case listOfString: List[String] => "a lists of strings"
    case listOfNumbers: List[Int] => "a list of numbers"
    case _ => ""
  }
  println(numbersMatch)
  // JVM trick question
  // type generics in pattern matching not working
  // type erasure
}
