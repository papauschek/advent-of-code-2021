import java.nio.file.{Files, Path}

object Advent23 {

  def main(args: Array[String]): Unit = {
    val world = World(Files.readString(Path.of("./data/input23.txt")).linesIterator.toList)

    var unorganized = world :: Nil
    var organized = List.empty[World]


  }

  private val noStopLocations = Set(Point(3, 1), Point(5, 1), Point(7, 1), Point(9, 1))

  private val destinationXByPod: Map[Char, Int] = Map(
    'A' -> 3,
    'B' -> 5,
    'C' -> 7,
    'D' -> 9
  )

  private val energyByPod: Map[Char, Int] = Map(
    'A' -> 1,
    'B' -> 10,
    'C' -> 100,
    'D' -> 1000
  )

  case class World(lines: Seq[String], energy: Int = 0) {

    val locations: Seq[Point] = {
      for {
        (line, y) <- lines.zipWithIndex
        (char, x) <- line.zipWithIndex if char >= 'A' && char <= 'D'
      } yield Point(x, y)
    }

    def isOrganized: Boolean = {
      locations.forall(l => destinationXByPod(get(l)) == l.x)
    }

    override def toString: String = {
      lines.mkString("\r\n") + s" Energy: $energy"
    }

    def options: Seq[World] = {
      locations.flatMap {
        location =>
          val spaces = availableSpaces(location).filterNot(l => l == location || noStopLocations.contains(l))
          val (hallSpaces, roomSpaces) = spaces.partition(_.y == 1)
          val isInHallway = location.y == 1
          val destinationX = destinationXByPod(get(location))
          val roomHasWrongPod = locations.exists(l => l.y >= 2 && l.x == destinationX && destinationXByPod(get(l)) != l.x)
          val destinationRoomSpaces = roomSpaces.filter(s => s.x == destinationX && !roomHasWrongPod)
          val possibleHallSpaces = hallSpaces.filterNot(_ => isInHallway)
          (destinationRoomSpaces ++ possibleHallSpaces).map(l => withLocation(location, l))
      }
    }

    def withLocation(from: Point, to: Point): World = {
      val char = get(from)
      val steps = if (from.y >= 2 && to.y >= 2 && from.x != to.x) {
        (from.y - 1).abs + (from.x - to.x).abs + (to.y - 1).abs
      } else {
        (to.x - from.x).abs + (to.y - from.y).abs
      }
      val addEnergy = energyByPod(char) * steps
      updated(from, '.').updated(to, char).copy(energy = energy + addEnergy)
    }

    def updated(point: Point, char: Char): World = {
      copy(lines = lines.updated(point.y, lines(point.y).updated(point.x, char)))
    }

    def availableSpaces(start: Point): List[Point] = {
      availableSpaces(start, start)
    }

    private def availableSpaces(start: Point, previousPoint: Point): List[Point] = {
      if (get(start) == '.' || start == previousPoint) {
        val neighbors = List(
          Point(start.x + 1, start.y),
          Point(start.x - 1, start.y),
          Point(start.x, start.y + 1),
          Point(start.x, start.y - 1)
        )
        start :: neighbors.flatMap {
          neighbor =>
            if (neighbor == previousPoint) {
              Nil
            } else {
              availableSpaces(neighbor, start)
            }
        }
      } else {
        Nil
      }
    }

    private def get(point: Point): Char = {
      lines(point.y)(point.x)
    }

  }

  case class Point(x: Int, y: Int)

}
