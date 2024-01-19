package day05

object Day05 {
  lazy val fileInput: Array[String] = io.Source.fromInputStream(getClass.getResourceAsStream("input.txt")).mkString.trim.filter(_ != '\r').split("\n")
  lazy val testInput: Array[String] = io.Source.fromInputStream(getClass.getResourceAsStream("testInput.txt")).mkString.trim.filter(_ != '\r').split("\n")


  def main(args: Array[String]): Unit = {
    val input = fileInput.map(_.toInt)
    println("HelloWorld")

    println("part 1: " + f(input.clone(), false))
    println("part 2: " + f(input.clone(), true))

  }

  def f(input: Array[Int], partTwo: Boolean): Int = {
    var i = 0
    var c = 0
    while (input.indices.contains(i)) {
      c = c + 1
      val v = input(i)
      if (v >= 3 && partTwo) {
        input(i) = input(i) - 1
      } else {
        input(i) = input(i) + 1
      }
      i = i + v
    }
    c

  }

}
