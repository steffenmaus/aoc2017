package day15

object Day15 {

  lazy val fileInput: Array[String] = io.Source.fromInputStream(getClass.getResourceAsStream("input.txt")).mkString.trim.filter(_ != '\r').split("\n")
  lazy val testInput: Array[String] = io.Source.fromInputStream(getClass.getResourceAsStream("testInput.txt")).mkString.trim.filter(_ != '\r').split("\n")

  val R: Long = 2147483647L
  val SMALL_R: Long = 65536L

  def main(args: Array[String]): Unit = {
    val input = fileInput

    val values = input.map(_.split(" ").last.toLong)

    val aMult = 16807
    val bMult = 48271
    val aDiv = 4
    val bDiv = 8

    var a = values(0)
    var b = values(1)
    var count = 0
    for (_ <- 0 until 40000000) {
      a = getNextVal(a, aMult)
      b = getNextVal(b, bMult)
      if (isMatch(a, b)) {
        count = count + 1
      }
    }
    println("part 1: " + count)


    a = values(0)
    b = values(1)
    count = 0
    for (_ <- 0 until 5000000) {
      a = getNextVal2(a, aMult, aDiv)
      b = getNextVal2(b, bMult, bDiv)
      if (isMatch(a, b)) {
        count = count + 1
      }
    }
    println("part 2: " + count)

  }

  def isMatch(a: Long, b: Long): Boolean = {
    a % SMALL_R == b % SMALL_R
  }

  def getNextVal(currentVal: Long, multiplier: Int): Long = {
    (currentVal * multiplier) % R
  }

  def getNextVal2(currentVal: Long, multiplier: Int, divider: Int): Long = {
    val temp = getNextVal(currentVal, multiplier)
    if (temp % divider == 0) {
      temp
    } else {
      getNextVal2(temp, multiplier, divider)
    }
  }

}
