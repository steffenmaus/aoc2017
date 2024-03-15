package day13

object Day13 {
  lazy val fileInput: Array[String] = io.Source.fromInputStream(getClass.getResourceAsStream("input.txt")).mkString.trim.filter(_ != '\r').split("\n")
  lazy val testInput: Array[String] = io.Source.fromInputStream(getClass.getResourceAsStream("testInput.txt")).mkString.trim.filter(_ != '\r').split("\n")

  def main(args: Array[String]): Unit = {
    val input = fileInput
    val distance = input.map(_.split(' ')(1).toInt).max
    val maxRange = input.last.split(':').head.toInt

    val m = input.map(_.split(": ").map(_.toInt)).map(t => (t(0), t(1))).toMap

    var count = 0
    var time = 0

    while (time <= maxRange) {
      if (m.keySet.contains(time)) {
        if (isHit(time, m(time), 0)) {
          count = count + (time * m(time))
        }
      }
      time = time + 1
    }
    println("part 1: " + count)


    var shift = 0
    var hit = true
    while (hit) {
      hit = false
      shift = shift + 1
      var time = 0
      while (time <= maxRange && !hit) {
        if (m.keySet.contains(time)) {
          if (isHit(time + shift, m(time), 0)) {
            hit = true
          }
        }
        time = time + 1
      }
    }
    println("part 2: " + shift)

  }

  def isHit(time: Int, width: Int, pos: Int): Boolean = {
    val t2 = time % (width * 2 - 2)

    if (t2 == pos || width * 2 - t2 == pos) {
      return true
    }
    false
  }


}
