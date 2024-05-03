package day17

object Day17 {

  def main(args: Array[String]): Unit = {

    val stepSize = 335
    var l = List(0)
    var pos = 0

    for (i <- 1 to 2017) {
      pos = (pos + stepSize) % l.length + 1
      val sl = l.splitAt(pos)
      l = sl._1.appended(i).appendedAll(sl._2)
    }
    val i2017 = l.indexWhere(_ == 2017)
    println("part 1: " + l(i2017 + 1))

    var p2 = 0
    pos = 0
    for (i <- 1 to 50000000) {
      pos = (pos + stepSize) % i
      if (pos == 0) {
        p2 = i
      }
      pos = pos + 1
    }
    println("part 2: " + p2)

  }

}
