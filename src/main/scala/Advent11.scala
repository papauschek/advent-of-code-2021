import java.nio.file.{Files, Path}

object Advent11 {

  def run(): Unit = {
    var lines = Files.readString(Path.of("./data/input11.txt")).linesIterator.toSeq
    val flashes = LazyList.iterate(1)(_ + 1).find {
      _ =>
        lines = iteration(lines)
        lines.forall(_.forall(_ == '0'))
    }
    println(flashes) // 232
    println(lines.mkString("", "\r\n", "\r\n"))
  }

  private def iteration(lines: Seq[String]): Seq[String] = {
    val energized = increaseEnergy(lines, 0, 0, 10, 10)
    val flashed = applyFlashes(energized)
    flashed.map(line => line.replace('-', '0'))
  }

  private def applyFlashes(lines: Seq[String]): Seq[String] = {
    var population = lines
    while (population.exists(_.exists(_ == 'X'))) {
      for {
        (line, y) <- population.zipWithIndex if line.contains('X')
      } yield {
        val x = line.indexOf('X')
        population = increaseEnergy(population, x - 1, y - 1, 3, 3)
      }
    }
    population
  }

  private def increaseEnergy(lines: Seq[String], minX: Int, minY: Int, width: Int, height: Int): Seq[String] = {
    for {
      (line, y) <- lines.zipWithIndex
    } yield {
      line.zipWithIndex.map {
        case (octopus, x) =>
          if (!octopus.isDigit
            || y < minY || y >= minY + height
            || x < minX || x >= minX + width) {
            if (minX == x - 1 && minY == y - 1 && height == 3) {
              '-'
            } else {
              octopus
            }
          } else {
            val digit = octopus.asDigit
            if (digit == 9) {
              'X'
            } else {
              (digit + 1).toString.head
            }
          }
      }.mkString
    }
  }

}
