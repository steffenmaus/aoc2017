package day10

object Day10 {
  lazy val fileInput: Array[String] = io.Source.fromInputStream(getClass.getResourceAsStream("input.txt")).mkString.trim.filter(_ != '\r').split("\n")

  def main(args: Array[String]): Unit = {
    val input = fileInput.head.split(',').map(_.trim.toInt).toList
    val inputAscii = fileInput.head.map(_.toInt).appendedAll(List(17, 31, 73, 47, 23)).toList

    val a = f(input, 1, 256).take(2).product
    println("part 1: " + a)

    val b = f(inputAscii, 64, 256)
      .grouped(16)
      .map(g => g.reduce(_ ^ _))
      .map(x => x.toHexString)
      .map(s => if (s.length == 1) "0" + s else s)
      .reduce(_ + _)
    println("part 2: " + b)

  }

  def f(lengths: List[Int], cycles: Int, size: Int): List[Int] = {
    var pos = 0
    var skipSize = 0
    var knots: List[Int] = (0 until size).toList

    for (_ <- 0 until cycles) {
      for (len <- lengths) {
        if (pos + len < size) {
          val reversedSlice = knots.slice(pos, pos + len).reverse
          knots = knots.take(pos).appendedAll(reversedSlice).appendedAll(knots.drop(pos + len))
        } else {
          val reversedSlice = knots.drop(pos).appendedAll(knots.take((pos + len) % size)).reverse
          val reversedSliceLeft = reversedSlice.takeRight((pos + len) - size)
          val reversedSliceRight = reversedSlice.dropRight((pos + len) - size)
          knots = reversedSliceLeft.appendedAll(knots.slice((pos + len) % size, pos)).appendedAll(reversedSliceRight)
        }
        pos = (pos + len + skipSize) % size
        skipSize = skipSize + 1
      }
    }
    knots

  }

}
