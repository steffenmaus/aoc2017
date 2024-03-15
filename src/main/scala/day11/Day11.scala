package day11

object Day11 {
  lazy val fileInput: Array[String] = io.Source.fromInputStream(getClass.getResourceAsStream("input.txt")).mkString.trim.filter(_ != '\r').split("\n")

  def main(args: Array[String]): Unit = {
    val input = fileInput.head

    var current = (0, 0)
    var max = 0

    for (a <- input.split(',')) {
      a match {
        case "n" => current = (current._1, current._2 + 2)
        case "s" => current = (current._1, current._2 - 2)
        case "ne" => current = (current._1 + 1, current._2 + 1)
        case "se" => current = (current._1 + 1, current._2 - 1)
        case "sw" => current = (current._1 - 1, current._2 - 1)
        case "nw" => current = (current._1 - 1, current._2 + 1)
      }
      max = Math.max(dist(current), max)
    }

    println("part 1: " + dist(current))
    println("part 2: " + max)

  }

  def dist(t: (Int, Int)): Int = {
    Math.abs(t._1) + (Math.max(0, (Math.abs(t._2) - Math.abs(t._1))) / 2)
  }

}
