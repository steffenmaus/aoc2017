package day23

import scala.collection.mutable

object Day23 {
  lazy val fileInput: Array[String] = io.Source.fromInputStream(getClass.getResourceAsStream("input.txt")).mkString.trim.filter(_ != '\r').split("\n")

  def main(args: Array[String]): Unit = {
    println("part 1: " + part1(fileInput))
    println("part 2: " + part2())
  }

  private def part1(input: Array[String]): Int = {
    val registers: mutable.HashMap[Char, Int] = mutable.HashMap.empty

    val instructions = input.map(line => {
      val s = line.split(' ')
      registers.addOne((s(1).head, 0))
      (s(0), s(1), s(2))
    })
    registers.addOne(('1', 1))

    var count = 0
    var i = 0
    while (i < 32) {
      val line = instructions(i)
      line._1 match {
        case "set" =>
          val c = line._2.head
          if (line._3.forall(_.isDigit) || line._3.startsWith("-")) {
            registers.addOne((c, line._3.toInt))
          } else {
            registers.addOne((c, registers(line._3.head)))
          }

        case "sub" =>
          val c = line._2.head
          val old = registers(c)
          if (line._3.forall(_.isDigit) || line._3.startsWith("-")) {
            registers.addOne((c, old - line._3.toInt))
          } else {
            registers.addOne((c, old - registers(line._3.head)))
          }

        case "mul" =>
          count = count + 1
          val c = line._2.head
          val old = registers(c)
          if (line._3.forall(_.isDigit) || line._3.startsWith("-")) {
            registers.addOne((c, old * line._3.toInt))
          } else {
            registers.addOne((c, old * registers(line._3.head)))
          }

        case "jnz" =>
          val c = line._2.head
          val old = registers(c)
          if (old != 0) {
            if (line._3.forall(_.isDigit) || line._3.startsWith("-")) {
              i = i + line._3.toInt - 1
            } else {
              i = i + registers(line._3.head) - 1
            }
          }
      }
      i = i + 1
    }
    count
  }


  private def part2(): Int = {

    var b = 84 * 100 + 100000
    val c = b + 17000
    var g = 1
    var h = 0

    while (true) {
      var f = 1
      var d = 2
      while (g != 0) {
        val maxE = b
        if (b % d == 0 && d * maxE > b) {
          f = 0
        }
        d = d + 1
        g = d - b
      }
      if (f == 0) {
        h = h + 1
      }
      g = b - c
      if (g == 0) {
        return h
      }
      b = b + 17
    }
    h
  }


}
