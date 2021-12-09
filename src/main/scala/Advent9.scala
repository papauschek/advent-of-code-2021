import java.nio.file.{Files, Path}
import scala.annotation.tailrec

object Advent9 {

  def run(): Unit = {
    val lines = Files.readString(Path.of("./data/input9.txt")).linesIterator.toList
    val rows = lines.map(_.map(_.asDigit).toList)
    val firstRow = rows.head

    val lowPoints = (for {
      row <- rows.indices
      column <- firstRow.indices
    } yield {
      val neighbors = getNeighbors(row, column, rows.length, firstRow.length)
      val minNeighbor = neighbors.map(p => rows(p.y)(p.x)).min
      val value = rows(row)(column)
      if (value < minNeighbor) {
        Some(Point(column, row))
      } else {
        None
      }
    }).flatten

    val basins = lowPoints.map {
      point => getBasin(rows.length, firstRow.length, rows, Set(point), Set.empty).size
    }

    val biggestBasins = basins.sorted.reverse.take(3)
    println(biggestBasins.product)
  }

  @tailrec
  private def getBasin(rowCount: Int, columnCount: Int,
                       rows: List[List[Int]],
                       openPoints: Set[Point],
                       visitedPoints: Set[Point]): Set[Point] = {

    val newVisitedPoints = visitedPoints ++ openPoints

    val newPoints = openPoints.flatMap {
      point =>
        getNeighbors(point.y, point.x, rowCount, columnCount).filter {
          neighbor =>
            val neighborValue = rows(neighbor.y)(neighbor.x)
            neighborValue < 9 && !newVisitedPoints.contains(neighbor)
        }
    }

    val newOpenPoints = newPoints -- newVisitedPoints
    if (newOpenPoints.isEmpty) {
      newVisitedPoints
    } else {
      getBasin(rowCount, columnCount, rows, newOpenPoints, newVisitedPoints)
    }
  }

  case class Point(x: Int, y: Int)

  private def getNeighbors(row: Int, column: Int, rowCount: Int, columnCount: Int): Seq[Point] = {
    List(
      Point(column, row - 1),
      Point(column + 1, row),
      Point(column, row + 1),
      Point(column - 1, row)
    ).filter {
      point => point.x >= 0 && point.y >= 0 && point.x < columnCount && point.y < rowCount
    }
  }

}
