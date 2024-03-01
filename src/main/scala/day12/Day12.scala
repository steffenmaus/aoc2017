package day12

import scala.collection.mutable

object Day12 {
  lazy val fileInput: Array[String] = io.Source.fromInputStream(getClass.getResourceAsStream("input.txt")).mkString.trim.filter(_ != '\r').split("\n")
  lazy val testInput: Array[String] = io.Source.fromInputStream(getClass.getResourceAsStream("testInput.txt")).mkString.trim.filter(_ != '\r').split("\n")

  def main(args: Array[String]): Unit = {
    val input = fileInput
    println("HelloWorld")

    val knotes: mutable.Set[Int] = mutable.HashSet.empty
    val edges: mutable.Map[Int, List[Int]] = mutable.HashMap.empty

    input.foreach(l => {
      val a = l.split(" ").head.toInt
      knotes.addOne(a)
      val s = l.split(">")(1).split(',').map(_.trim).map(_.toInt)
      edges.addOne(a, s.toList)
    })

    var i = 0
    while (knotes.nonEmpty) {
      i = i + 1
      val completed: mutable.Set[Int] = mutable.HashSet.empty
      val open: mutable.Set[Int] = mutable.HashSet.empty
      val seed = knotes.head
      open.addOne(seed)

      while (open.nonEmpty) {
        val x = open.head
        open.remove(x)
        edges(x).foreach(n => {
          if (!completed.contains(n)) {
            completed.addOne(n)
            open.addOne(n)
          }
        })
      }
      completed.foreach(knotes.remove)
      if (seed == 0) {
        println("part 1: " + completed.size)
      }
    }

    println("part 2: " + i)

  }

}
