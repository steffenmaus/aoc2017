package day09

import scala.collection.mutable

object Day09 {
  lazy val fileInput: Array[String] = io.Source.fromInputStream(getClass.getResourceAsStream("input.txt")).mkString.trim.filter(_ != '\r').split("\n")


  def main(args: Array[String]): Unit = {
    val input = fileInput.head

    val withoutCancellations = new mutable.StringBuilder()
    var skipNext = false
    for (c <- input) {
      if (!skipNext) {
        if (c.equals('!')) {
          skipNext = true
        } else {
          withoutCancellations.addOne(c)
        }
      } else {
        skipNext = false
      }
    }

    val withoutGarbage = new mutable.StringBuilder()
    var withinGarbage = false
    var garbageCount = 0
    for (c <- withoutCancellations) {
      if (!withinGarbage) {
        if (c.equals('<')) {
          withinGarbage = true
        } else {
          withoutGarbage.addOne(c)
        }
      } else {
        if (c.equals('>')) {
          withinGarbage = false
        } else {
          garbageCount = garbageCount + 1
        }
      }
    }

    var count = 0
    var depth = 0
    for (t <- withoutGarbage) {
      if (t.equals('{')) {
        depth = depth + 1
        count = count + depth
      } else if (t.equals('}')) {
        depth = depth - 1
      }
    }

    println("part1: " + count)
    println("part2: " + garbageCount)

  }

}
