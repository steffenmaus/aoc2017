package day01

object Day01 {
  lazy val fileInput: Array[String] = io.Source.fromInputStream(getClass.getResourceAsStream("input.txt")).mkString.trim.filter(_ != '\r').split("\n")

  def main(args: Array[String]): Unit = {
    val input = fileInput.head.map(_.asDigit)

    var a = 0
    var b = 0
    for (i <- input.indices) {
      if (input(i) == input((i + 1) % input.size)) {
        a = a + input(i)
      }
      if (input(i) == input((i + input.size / 2) % input.size)) {
        b = b + input(i)
      }
    }

    println("part 1: " + a)
    println("part 2: " + b)
  }

}
