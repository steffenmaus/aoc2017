package day04

object Day04 {
  lazy val fileInput: Array[String] = io.Source.fromInputStream(getClass.getResourceAsStream("input.txt")).mkString.trim.filter(_ != '\r').split("\n")
  lazy val testInput: Array[String] = io.Source.fromInputStream(getClass.getResourceAsStream("testInput.txt")).mkString.trim.filter(_ != '\r').split("\n")


  def main(args: Array[String]): Unit = {
    val input = fileInput

    val a = input.map(l => l.split(" ")).count(words => words.toSet.size == words.length)
    val b = input.map(l => l.split(" ")).count(words => words.map(_.sorted).toSet.size == words.length)
    println("part 1: " + a)
    println("part 2: " + b)
  }

}
