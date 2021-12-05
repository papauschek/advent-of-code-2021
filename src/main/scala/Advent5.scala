import java.nio.file.{Files, Path}

object Advent5 {

  def run(): Unit = {
    val file = Files.readString(Path.of("./data/input5.txt")).linesIterator.toList
    val lines = file.map(parseLine)
    val points = lines.flatMap(_.points)
    val count = points.groupBy(p => p).count { case (_, list) => list.length >= 2 }
    println(count)
  }

  private def parseLine(text: String): Line = {
    val parts = text.split(" -> ")
    val List(p1, p2) = parts.map(parsePoint).toList
    Line(p1, p2)
  }

  private def parsePoint(text: String): Point = {
    val List(x, y) = text.split(',').map(_.toInt).toList
    Point(x, y)
  }

  case class Point(x: Int, y: Int)

  case class Line(p1: Point, p2: Point) {

    def points: Seq[Point] = {
      val (dx, dy) = (p2.x - p1.x, p2.y - p1.y)
      val steps = Math.max(dx.abs, dy.abs)
      val (stepX, stepY) = (dx.sign, dy.sign)
      (0 to steps).map(step => Point(p1.x + step * stepX, p1.y + step * stepY))
    }

  }


}
