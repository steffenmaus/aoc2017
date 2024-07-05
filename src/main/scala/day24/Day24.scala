package day24

object Day24 {

  lazy val fileInput: Array[String] = io.Source.fromInputStream(getClass.getResourceAsStream("input.txt")).mkString.trim.filter(_ != '\r').split("\n")
  lazy val testInput: Array[String] = io.Source.fromInputStream(getClass.getResourceAsStream("testInput.txt")).mkString.trim.filter(_ != '\r').split("\n")

  def main(args: Array[String]): Unit = {
    val input = fileInput

    val ports: Set[(Int, Int)] = input.map(line => {
      val sl = line.split('/')
      (sl.head.toInt, sl.last.toInt)
    }).toSet

    val r = f(ports, 0, List.empty)

    val p1 = r.map(_._2).max
    val maxSize = r.map(_._1).max
    val p2 = r.filter(_._1.equals(maxSize)).map(_._2).max

    println("Part 1: " + p1)
    println("Part 2: " + p2)

  }

  def f(set: Set[(Int, Int)], p: Int, current: List[(Int, Int)]): Set[(Int, Int)] = {
    if (set.exists(s => s._1.equals(p) || s._2.equals(p))) {
      return Set((current.length, current.map(x => x._1 + x._2).sum))
    }

    set.filter(s => s._1.equals(p) || s._2.equals(p)).flatMap(s => {
      if (s._1.equals(p)) {
        f(set.filterNot(_.equals(s)), s._2, current.appended(s))
      } else {
        f(set.filterNot(_.equals(s)), s._1, current.appended(s))
      }
    })
  }

}
