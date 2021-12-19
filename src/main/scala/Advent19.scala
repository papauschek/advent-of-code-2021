import java.nio.file.{Files, Path}

object Advent19 {

  def main(args: Array[String]): Unit = {
    val inputBlocks = Files.readString(Path.of("./data/input19.txt")).split("\n\n").toSeq
    val scanners = inputBlocks.zipWithIndex.map { case (lines, index) => parseScanner(index, lines.linesIterator.toSeq) }

    val baseScanner = scanners.head
    var world = World(baseScanner, baseScanner :: Nil, scanners.tail)
    while (world.unaligned.nonEmpty) {
      world = alignNextScanner(world)
      println(world)
    }

    val distances = for {
      x <- world.aligned
      y <- world.aligned
    } yield x.position.distance(y.position)
    println(distances.max) // 10685
  }

  private def alignNextScanner(world: World): World = {
    (for {
      unalignedScanner <- world.unaligned.iterator
      alignedScanner <- world.base.getAlignedScanner(unalignedScanner)
    } yield {
      World(
        base = world.base.copy(points = world.base.points ++ alignedScanner.points),
        aligned = world.aligned :+ alignedScanner,
        unaligned = world.unaligned.filterNot(_ == unalignedScanner)
      )
    }).next()
  }

  private def parseScanner(index: Int, lines: Seq[String]): Scanner = {
    Scanner(index, points = lines.tail.map(parsePoint).toSet)
  }

  private def parsePoint(line: String): Point = {
    Point(vector = line.split(',').toVector.map(_.toInt))
  }

  case class World(base: Scanner, aligned: Seq[Scanner], unaligned: Seq[Scanner]) {
    override def toString: String = s"Aligned: ${aligned.map(_.number).mkString(",")}: ${base.points.size} Beacons"
  }

  case class Scanner(number: Int, points: Set[Point], position: Point = Point(Vector(0, 0, 0))) {

    def getAlignedScanner(unaligned: Scanner): Option[Scanner] = {
      (for {
        orientation <- unaligned.orientations
        scannerPosition <- getScannerPosition(orientation)
      } yield {
        orientation.copy(
          points = orientation.points.map(point => point.add(scannerPosition)),
          position = scannerPosition
        )
      }).nextOption()
    }

    private def getScannerPosition(other: Scanner): Option[Point] = {
      (for {
        p1 <- points.iterator
        p2 <- other.points if isMatch(other, p1.subtract(p2))
      } yield {
        p1.subtract(p2)
      }).nextOption()
    }

    private def isMatch(other: Scanner, otherPosition: Point): Boolean = {
      val MIN_OVERLAP_BEACONS: Int = 12
      val matchCount = other.points.count(point => points.contains(point.add(otherPosition)))
      matchCount >= MIN_OVERLAP_BEACONS
    }

    private def orientations: Iterator[Scanner] = {
      for {
        dx <- Seq(0, 1, 2).iterator
        dy <- Seq(0, 1, 2) if dy != dx
        dz <- Seq(0, 1, 2) if dz != dy && dz != dx
        mx <- Seq(1, -1)
        my <- Seq(1, -1)
        mz <- Seq(1, -1)
      } yield {
        copy(points = points.map {
          point =>
            val (x, y, z) = (point.vector(dx), point.vector(dy), point.vector(dz))
            Point(Vector(x * mx, y * my, z * mz))
        })
      }
    }

  }

  case class Point(vector: Vector[Int]) {
    def x: Int = vector(0)
    def y: Int = vector(1)
    def z: Int = vector(2)
    def subtract(other: Point): Point = Point(Vector(x - other.x, y - other.y, z - other.z))
    def add(other: Point): Point = Point(Vector(x + other.x, y + other.y, z + other.z))
    def distance(other: Point): Int = (x - other.x).abs + (y - other.y).abs + (z - other.z).abs
  }

}
