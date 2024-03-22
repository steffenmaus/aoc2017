package day14

import scala.collection.mutable

object Day14 {
  lazy val fileInput = "ugkiagan"
  lazy val testInput = "flqrgnkx"

  def main(args: Array[String]): Unit = {
    val input = fileInput
    println("HelloWorld")

    val openPos: mutable.Set[(Int, Int)] = mutable.Set.empty

    for (y <- 0 until 128) {
      val l = hexToBin(knotHash(input + "-" + y))
      openPos.addAll(l.zipWithIndex.filter(_._1.equals('1')).map(_._2).map(x => (x, y)))
    }
    println("part 1: " + openPos.size)

    var regions = 0
    while (openPos.nonEmpty) {
      regions = regions + 1
      val regionSeed = openPos.head
      openPos.remove(regionSeed)

      val nbs: mutable.Set[(Int, Int)] = mutable.Set.empty
      nbs.addAll(getValidNeighbours(regionSeed).filter(openPos.contains))
      while (nbs.nonEmpty) {
        val n = nbs.head
        nbs.remove(n)
        openPos.remove(n)
        nbs.addAll(getValidNeighbours(n).filter(openPos.contains))
      }
    }

    println("part 2: " + regions)

  }

  def getValidNeighbours(pos: (Int, Int)): List[(Int, Int)] = {
    List((0, 1), (1, 0), (-1, 0), (0, -1)).map(t => (t._1 + pos._1, t._2 + pos._2)).filter(t => t._1 >= 0 && t._2 >= 0 && t._1 < 128 && t._2 < 128)
  }

  def hexToBin(in: String): String = {
    "0000".appendedAll(BigInt(in, 16).toString(2)).takeRight(in.length * 4)
  }

  def knotHash(in: String): String = {
    f(in.map(_.toInt).appendedAll(List(17, 31, 73, 47, 23)).toList, 64, 256)
      .grouped(16)
      .map(g => g.reduce(_ ^ _))
      .map(x => x.toHexString)
      .map(s => if (s.length == 1) "0" + s else s)
      .reduce(_ + _)
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
