package day16

import scala.collection.mutable

object Day16 {
  lazy val fileInput: Array[String] = io.Source.fromInputStream(getClass.getResourceAsStream("input.txt")).mkString.trim.filter(_ != '\r').split("\n")

  def main(args: Array[String]): Unit = {
    val input = fileInput.head.split(',')
    val TOTAL_COUNT = 1000000000

    var order = "abcdefghijklmnop".toArray
    val m = mutable.HashMap[Int, String]()
    m.addOne(0, order.mkString)

    var count = 0
    while (count < TOTAL_COUNT) {
      input.foreach(i => {
        if (i.startsWith("s")) {
          val n = i.drop(1).toInt
          val r = order.takeRight(n)
          val l = order.take(order.length - n)
          order = r.appendedAll(l)
        }
        else if (i.startsWith("x")) {
          val a = i.drop(1).split('/').head.toInt
          val b = i.drop(1).split('/')(1).toInt
          val x = order(a)
          val y = order(b)
          order(b) = x
          order(a) = y
        }
        else if (i.startsWith("p")) {
          val a = i.drop(1).split('/').head.head
          val b = i.drop(1).split('/')(1).head
          val x = order.indexOf(a)
          val y = order.indexOf(b)
          order(x) = b
          order(y) = a
        }
      })

      count = count + 1
      val s = order.mkString
      if (count == 1) {
        println("part 1: " + s)
      }
      if (m.values.toSet.contains(s)) {
        val r = TOTAL_COUNT % count
        println("part 2: " + m(r))
        System.exit(0)
      } else {
        m.addOne(count, s)
      }
    }
  }

}
