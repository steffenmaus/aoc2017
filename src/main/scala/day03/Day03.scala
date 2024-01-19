package day03

object Day03 {

  def main(args: Array[String]): Unit = {
    val input = 289326

    var r = 0
    var prevVolume = 0
    var volume = 1
    while (volume < input) {
      r = r + 1
      prevVolume = volume
      volume = (1 + r * 2) * (1 + r * 2)
    }
    val sliceSize = (volume - prevVolume) / 4
    val stepsInSlice = (input - prevVolume) % sliceSize
    val some = if (stepsInSlice <= r) stepsInSlice else sliceSize - stepsInSlice

    println("part 1: " + (2 * r - some))

    //first approach ends here...


    val dimension = 2 * (r + 1)

    val grid: Array[Array[Int]] = new Array[Array[Int]](dimension)
    for (y <- grid.indices) {
      grid(y) = Array.fill(dimension) {
        0
      }
    }

    val startPoint = (dimension / 2, dimension / 2)
    var currentPoint = startPoint
    var currentDir = 3 //R U L D
    var currentSteps = 0
    var currentLimit = 0

    grid(currentPoint._2)(currentPoint._1) = 1

    var i = 1
    var skip = false
    while (grid.indices.contains(currentPoint._2) && grid.head.indices.contains(currentPoint._1)) {
      if (currentSteps < currentLimit) { //normal step
        i = i + 1
        currentSteps = currentSteps + 1
        if (currentDir == 0) {
          currentPoint = (currentPoint._1 + 1, currentPoint._2)
        } else if (currentDir == 1) {
          currentPoint = (currentPoint._1, currentPoint._2 - 1)
        } else if (currentDir == 2) {
          currentPoint = (currentPoint._1 - 1, currentPoint._2)
        } else if (currentDir == 3) {
          currentPoint = (currentPoint._1, currentPoint._2 + 1)
        }

        if (!skip) {
          val adj = List((-1, -1), (-1, 0), (-1, 1), (0, -1), (0, 1), (1, -1), (1, 0), (1, 1))
            .map(t => (t._1 + currentPoint._1, t._2 + currentPoint._2))
            .filter(t => t._1 >= 0 && t._2 >= 0 && t._1 < dimension && t._2 < dimension)

          val adjSum = adj.map(t => grid(t._2)(t._1)).sum
          grid(currentPoint._2)(currentPoint._1) = adjSum
          if (adjSum > input) {
            println("part 2: " + adjSum)
            skip = true
          }
        }

      } else { //no move, only rotate
        if (currentDir == 0) {
          currentDir = 1
          currentSteps = 0
        } else if (currentDir == 1) {
          currentDir = 2
          currentSteps = 0
          currentLimit = currentLimit + 1
        } else if (currentDir == 2) {
          currentDir = 3
          currentSteps = 0
        } else if (currentDir == 3) {
          currentDir = 0
          currentSteps = 0
          currentLimit = currentLimit + 1
        }

      }
      if (i == input) {
        val dis = math.abs(startPoint._1 - currentPoint._1) + math.abs(startPoint._2 - currentPoint._2)
        println("part 1: " + dis)
      }

    }


  }

}
