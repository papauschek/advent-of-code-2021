import java.nio.file.{Files, Path}
import scala.collection.mutable

object Advent15 {

  def run(): Unit = {
    val lines = Files.readString(Path.of("./data/input15.txt")).linesIterator.filter(_.nonEmpty).toList

    // partial map (top left part)
    val topLeftRisks: Map[Point, Int] = (for {
      (line, y) <- lines.zipWithIndex
      x <- line.indices
    } yield (Point(x, y), line(x).asDigit)).toMap

    // repeat map 25x times
    val risks: Map[Point, Int] = (for {
      x <- 0 until 500
      y <- 0 until 500
    } yield {
      val baseRisk = topLeftRisks(Point(x % 100, y % 100))
      val extra = x / 100 + y / 100
      val newRisk = (baseRisk - 1 + extra) % 9 + 1
      (Point(x, y), newRisk)
    }).toMap

    // define start and destination
    val startingPoint = Point(0, 0)
    val endPoint = Point(499, 499)

    // Mark all nodes unvisited, assign tentative distance value
    val initialUnvisitedPoints: List[Point] = (risks.keySet - startingPoint).toList
    val distances: mutable.Map[Point, Int] = mutable.Map.from(initialUnvisitedPoints.map(point => (point, Int.MaxValue)))

    var currentPoint = startingPoint
    var currentDistance = 0
    while (currentPoint != endPoint) {

      // For the current node, consider all of its unvisited neighbors and calculate their tentative distances through the current node.
      getNeighbors(currentPoint).foreach {
        neighbor =>
          if (distances.contains(neighbor)) {
            val oldDistance = distances(neighbor)
            val tentativeDistance = currentDistance + risks(neighbor)
            if (oldDistance > tentativeDistance) {
              distances.update(neighbor, tentativeDistance)
            }
          }
      }

      // mark the current node as visited and remove it from the unvisited set. A visited node will never be checked again.
      distances.remove(currentPoint)
      val (minPoint, minDistance) = distances.minBy { case (_, distance) => distance }
      currentDistance = minDistance
      currentPoint = minPoint

      // monitor progress (takes ~ 10 min)
      if (distances.size % 1000 == 0) {
        println(distances.size / risks.size.toDouble, currentPoint, currentDistance)
      }
    }

    println(distances(endPoint)) // 3012
  }

  private def getNeighbors(point: Point): List[Point] = {
    var neighbors = List.empty[Point]
    if (point.x > 0) neighbors = Point(point.x - 1, point.y) :: neighbors
    if (point.y > 0) neighbors = Point(point.x, point.y - 1) :: neighbors
    if (point.x < 499) neighbors = Point(point.x + 1, point.y) :: neighbors
    if (point.y < 499) neighbors = Point(point.x, point.y + 1) :: neighbors
    neighbors
  }

  case class Point(x: Int, y: Int)

}
