package day07

import scala.collection.mutable

object Day07 {

  lazy val fileInput: Array[String] = io.Source.fromInputStream(getClass.getResourceAsStream("input.txt")).mkString.trim.filter(_ != '\r').split("\n")
  lazy val testInput: Array[String] = io.Source.fromInputStream(getClass.getResourceAsStream("testInput.txt")).mkString.trim.filter(_ != '\r').split("\n")


  val edges: mutable.HashMap[String, List[String]] = mutable.HashMap.empty
  val weights: mutable.HashMap[String, Int] = mutable.HashMap.empty
  val totalWeights: mutable.HashMap[String, Int] = mutable.HashMap.empty

  def main(args: Array[String]): Unit = {
    val input = fileInput

    input.map(_.split("->")).foreach(line => {
      if (line.length > 1) {
        edges.addOne(line(0).split(" ").head, line(1).split(',').map(_.trim).toList)
      }
      weights.addOne(line(0).split(" ").head, line(0).split(' ')(1).filter(_.isDigit).toInt)
    })

    val leftNodes = edges.keySet
    val rightNodes = edges.flatMap(_._2.toSet).toSet

    val startNode = leftNodes.diff(rightNodes).head
    println("part 1: " + startNode)

    calcTotalWeight(startNode)

    val unstables = edges.filter(e => {
      val l = e._2.map(e => totalWeights(e))
      l.size > 1 && l.distinct.size != 1
    })

    val parentEdge = unstables.filter(c => !c._2.exists(unstables.keySet.contains)).head._1
    val childs = edges(parentEdge).map(x => (x, totalWeights(x)))
    val criticalChild = childs.filter(c => childs.map(_._2).count(_.equals(c._2)) == 1).head._1
    val sibling = childs.filter(!_._1.equals(criticalChild)).head._1

    val delta = totalWeights(sibling) - totalWeights(criticalChild)
    val fixedWeight = weights(criticalChild) + delta

    println("part 2: " + fixedWeight)

  }


  def calcTotalWeight(node: String): Int = {
    if (!totalWeights.contains(node)) {
      if (edges.contains(node)) {
        val childs = edges(node)
        var sum = weights(node)
        for (c <- childs) {
          sum = sum + calcTotalWeight(c)
        }
        totalWeights.addOne(node, sum)
      } else {
        totalWeights.addOne(node, weights(node))
      }
    }
    totalWeights(node)
  }


}
