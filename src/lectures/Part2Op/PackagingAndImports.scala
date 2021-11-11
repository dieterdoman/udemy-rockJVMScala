package lectures.Part2Op

import lectures.Part1Basics.DefaultArgs.{savePicture => picture}

object PackagingAndImports extends App {
  val writer = new Writer("Daniel", "RockTheJvm", 2019)
  picture(width=920)

  //package object
  // rarely used
  sayHello
}
