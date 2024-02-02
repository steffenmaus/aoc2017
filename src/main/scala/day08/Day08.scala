package day08

import scala.collection.mutable

object Day08 {
  lazy val fileInput: Array[String] = io.Source.fromInputStream(getClass.getResourceAsStream("input.txt")).mkString.trim.filter(_ != '\r').split("\n")
  lazy val testInput: Array[String] = io.Source.fromInputStream(getClass.getResourceAsStream("testInput.txt")).mkString.trim.filter(_ != '\r').split("\n")


  def main(args: Array[String]): Unit = {
    val input = fileInput

    val values: mutable.Map[String, Int] = mutable.HashMap.empty

    input.foreach(line => values.addOne(line.split(" ")(4), 0))

    var maxVal = 0

    input.foreach(line => {
      val sl = line.split(" ")
      val targetDelta = if (sl(1).equals("inc")) sl(2).toInt else sl(2).toInt * -1

      val reg = sl(4)
      val op = sl(5)
      val v = sl(6).toInt

      if (op.equals(">") && values(reg) > v ||
        op.equals("<") && values(reg) < v ||
        op.equals(">=") && values(reg) >= v ||
        op.equals("<=") && values(reg) <= v ||
        op.equals("!=") && values(reg) != v ||
        op.equals("==") && values(reg) == v) {
        values(sl(0)) = values(sl(0)) + targetDelta
        maxVal = Math.max(maxVal, values.values.max)
      }

    })

    println("part 1: " + values.values.max)
    println("part 2: " + maxVal)
  }


}
