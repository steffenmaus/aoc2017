package day06

import scala.collection.mutable

object Day06 {
  lazy val fileInput: Array[String] = io.Source.fromInputStream(getClass.getResourceAsStream("input.txt")).mkString.trim.filter(_ != '\r').split("\n")
  lazy val testInput: Array[String] = io.Source.fromInputStream(getClass.getResourceAsStream("testInput.txt")).mkString.trim.filter(_ != '\r').split("\n")

  def main(args: Array[String]): Unit = {
    val input = fileInput.head.split('	').map(_.toInt)


    val knownStates: mutable.Set[List[Int]] = mutable.HashSet.empty
    val stateMap: mutable.Map[List[Int], Int] = mutable.HashMap.empty

    var i = 0
    while (!knownStates.contains(input.toList)) {
      knownStates.addOne(input.toList)
      stateMap.addOne(input.toList, i)
      setNextState(input)
      i = i + 1
    }

    println("part 1: " + knownStates.size)
    println("part 2: " + (knownStates.size - stateMap(input.toList)))

  }

  def setNextState(in: Array[Int]): Unit = {
    var i = in.indexWhere(_.equals(in.max))
    var value = in(i)
    in(i) = 0
    while (value > 0) {
      i = (i + 1) % in.length
      in(i) = in(i) + 1
      value = value - 1
    }
  }

}
