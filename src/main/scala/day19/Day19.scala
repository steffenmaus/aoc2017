package day19

import scala.collection.mutable.ListBuffer

object Day19 {
  lazy val fileInput: Array[String] = io.Source.fromInputStream(getClass.getResourceAsStream("input.txt")).mkString.filter(_ != '\r').split("\n")

  def main(args: Array[String]): Unit = {
    val input = fileInput

    val grid: Array[Array[Char]] = new Array(input.length)
    for (y <- grid.indices) {
      grid(y) = input(y).toCharArray
    }

    val KNOWN_SYMBOLS = Set('-', '|', '+', ' ')
    val sol: ListBuffer[Char] = ListBuffer.empty

    var current = (0, grid(0).indexWhere(_.equals('|')), 2)
    var steps = 0
    var stuck = false
    while (!stuck) {
      steps = steps + 1
      val s = grid(current._1)(current._2)
      if (!KNOWN_SYMBOLS.contains(s)) {
        sol.addOne(s)
      }

      if (current._3 == 0 && !grid(current._1 - 1)(current._2).equals(' ')) {
        current = (current._1 - 1, current._2, 0)
      } else if (current._3 == 2 && !grid(current._1 + 1)(current._2).equals(' ')) {
        current = (current._1 + 1, current._2, 2)
      } else if (current._3 == 1 && !grid(current._1)(current._2 + 1).equals(' ')) {
        current = (current._1, current._2 + 1, 1)
      } else if (current._3 == 3 && !grid(current._1)(current._2 - 1).equals(' ')) {
        current = (current._1, current._2 - 1, 3)
      } else if ((current._3 == 0 || current._3 == 2) && s.equals('+')) {
        if (!grid(current._1)(current._2 + 1).equals(' ')) {
          current = (current._1, current._2 + 1, 1)
        } else {
          current = (current._1, current._2 - 1, 3)
        }
      } else if ((current._3 == 1 || current._3 == 3) && s.equals('+')) {
        if (!grid(current._1 + 1)(current._2).equals(' ')) {
          current = (current._1 + 1, current._2, 2)
        } else {
          current = (current._1 - 1, current._2, 0)
        }
      } else {
        stuck = true
      }
    }

    println("part 1: " + sol.mkString)
    println("part 2: " + steps)

  }

}
