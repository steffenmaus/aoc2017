package day02

object Day02 {
  lazy val fileInput: Array[String] = io.Source.fromInputStream(getClass.getResourceAsStream("input.txt")).mkString.trim.filter(_ != '\r').split("\n")

  def main(args: Array[String]): Unit = {
    val input = fileInput

    val a = input.map(l => l.split("\t").map(_.toInt)).map(l => l.max - l.min).sum
    val b = input.map(l => l.split("\t").map(_.toInt)).map(l => {
      val t = l.toSet.subsets(2).filter(x => x.head % x.last == 0 || x.last % x.head == 0).toList.head.toList
      if (t.head > t.last) {
        t.head / t.last
      } else {
        t.last / t.head
      }
    }).sum

    println("part 1: " + a)
    println("part 2: " + b)
  }

}
