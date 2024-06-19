package day21

import scala.collection.mutable

object Day21 {
  lazy val fileInput: Array[String] = io.Source.fromInputStream(getClass.getResourceAsStream("input.txt")).mkString.trim.filter(_ != '\r').split("\n")
  lazy val testInput: Array[String] = io.Source.fromInputStream(getClass.getResourceAsStream("testInput.txt")).mkString.trim.filter(_ != '\r').split("\n")


  def main(args: Array[String]): Unit = {
    val input = fileInput

    val mapper: mutable.HashMap[String, String] = mutable.HashMap.empty

    input.map(l => l.filter(!_.equals('/')).split(" => ")).foreach(l => {
      val r0 = l.head
      val r90 = rotate90Right(r0)
      val r180 = rotate90Right(r90)
      val r270 = rotate90Right(r180)
      mapper.addOne(r0, l.last)
      mapper.addOne(r90, l.last)
      mapper.addOne(r180, l.last)
      mapper.addOne(r270, l.last)

      val v0 = flipV(r0)
      val v90 = rotate90Right(v0)
      val v180 = rotate90Right(v90)
      val v270 = rotate90Right(v180)
      mapper.addOne(v0, l.last)
      mapper.addOne(v90, l.last)
      mapper.addOne(v180, l.last)
      mapper.addOne(v270, l.last)

      val h0 = flipH(r0)
      val h90 = rotate90Right(h0)
      val h180 = rotate90Right(h90)
      val h270 = rotate90Right(h180)
      mapper.addOne(h0, l.last)
      mapper.addOne(h90, l.last)
      mapper.addOne(h180, l.last)
      mapper.addOne(h270, l.last)
    })

    val seed = ".#...####"
    var grids: List[String] = List(seed)

    for (i <- 1 to 18) {
      if (grids.head.length == 36) {
        grids = grids.flatMap(g => {
          List("" + g(0) + g(1) + g(6) + g(7),
            "" + g(2) + g(3) + g(8) + g(9),
            "" + g(4) + g(5) + g(10) + g(11),
            "" + g(12) + g(13) + g(18) + g(19),
            "" + g(14) + g(15) + g(20) + g(21),
            "" + g(16) + g(17) + g(22) + g(23),
            "" + g(24) + g(25) + g(30) + g(31),
            "" + g(26) + g(27) + g(32) + g(33),
            "" + g(28) + g(29) + g(34) + g(35))
        })
      }

      grids = grids.map(g => {
        if (g.length < 16) {
          mapper(g)
        } else {
          val s1 = mapper("" + g(0) + g(1) + g(4) + g(5))
          val s2 = mapper("" + g(2) + g(3) + g(6) + g(7))
          val s3 = mapper("" + g(8) + g(9) + g(12) + g(13))
          val s4 = mapper("" + g(10) + g(11) + g(14) + g(15))
          "" + s1.take(3) + s2.take(3) + s1.slice(3, 6) + s2.slice(3, 6) +
            s1.takeRight(3) + s2.takeRight(3) + s3.take(3) + s4.take(3) +
            s3.slice(3, 6) + s4.slice(3, 6) + s3.takeRight(3) + s4.takeRight(3)
        }
      })
      if (i == 5) {
        println("part 1: " + grids.map(_.count(_.equals('#'))).sum)
      }
    }
    println("part 2: " + grids.map(_.count(_.equals('#'))).sum)
  }


  def rotate90Right(s: String): String = {
    if (s.length == 9) {
      "" + s(6) + s(3) + s(0) + s(7) + s(4) + s(1) + s(8) + s(5) + s(2)
    } else {
      "" + s(2) + s(0) + s(3) + s(1)
    }
  }

  def flipV(s: String): String = {
    if (s.length == 9) {
      "" + s(6) + s(7) + s(8) + s(3) + s(4) + s(5) + s(0) + s(1) + s(2)
    } else {
      "" + s(2) + s(3) + s(0) + s(1)
    }
  }

  def flipH(s: String): String = {
    if (s.length == 9) {
      "" + s(2) + s(1) + s(0) + s(5) + s(4) + s(3) + s(8) + s(7) + s(6)
    } else {
      "" + s(1) + s(0) + s(3) + s(2)
    }
  }


}
