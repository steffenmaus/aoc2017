package day25

import scala.collection.mutable

object Day25 {

  lazy val fileInput: Array[Array[String]] = io.Source.fromInputStream(getClass.getResourceAsStream("input.txt")).mkString.trim.filter(_ != '\r').split("\n\n").map(_.split("\n"))
  lazy val testInput: Array[Array[String]] = io.Source.fromInputStream(getClass.getResourceAsStream("testInput.txt")).mkString.trim.filter(_ != '\r').split("\n\n").map(_.split("\n"))

  def main(args: Array[String]): Unit = {
    val input = fileInput

    val actives: mutable.Set[Int] = mutable.Set.empty
    val m: mutable.Map[String, ((Boolean, Int, String), (Boolean, Int, String))] = mutable.HashMap.empty

    input.drop(1).foreach(g => {
      val name = g.head.split(' ').last.dropRight(1)

      val a = g(2).split(' ').last.dropRight(1).equals("1")
      val b = if (g(3).contains("right")) 1 else -1
      val c = g(4).split(' ').last.dropRight(1)

      val a2 = g(6).split(' ').last.dropRight(1).equals("1")
      val b2 = if (g(7).contains("right")) 1 else -1
      val c2 = g(8).split(' ').last.dropRight(1)

      m.addOne((name, ((a, b, c), (a2, b2, c2))))
    })

    var steps = input.head.last.split(' ').dropRight(1).last.toInt
    var state = input.head.head.split(' ').last.dropRight(1)
    var pos = 0

    while (steps > 0) {
      steps = steps - 1
      val isActive = actives.contains(pos)
      actives.remove(pos)

      val op = m(state)
      val rop = if (isActive) op._2 else op._1

      if (rop._1) {
        actives.addOne(pos)
      }
      pos = pos + rop._2
      state = rop._3
    }

    println("Part 1: " + actives.size)
  }


}
