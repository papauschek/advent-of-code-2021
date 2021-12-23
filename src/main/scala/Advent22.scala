import java.nio.file.{Files, Path}

object Advent22 {

  def main(args: Array[String]): Unit = {
    val lines = Files.readString(Path.of("./data/input22.txt")).linesIterator.toList
    val commands = lines.map(parseCommand)

    var enabledCuboids = List.empty[Command]
    commands.foreach {
      command =>
        if (command.on) {
          enabledCuboids ++= enabledCuboids.foldLeft(command :: Nil)((added, existing) => added.flatMap(_.subtract(existing)))
        } else {
          enabledCuboids = enabledCuboids.flatMap(_.subtract(command))
        }
    }

    println(enabledCuboids.map(_.size).sum) // 1288707160324706
  }

  private def parseCommand(line: String): Command = {
    val List(switch, coords) = line.split(' ').toList
    val parts = coords.split(',').toList.map(_.substring(2).split("\\.\\.").toList.map(_.toInt))
    val List(List(minX, maxX), List(minY, maxY), List(minZ, maxZ)) = parts
    Command(
      min = Point(minX, minY, minZ),
      max = Point(maxX, maxY, maxZ),
      on = switch == "on"
    )
  }

  case class Command(min: Point, max: Point, on: Boolean = true) {

    def size: Long = {
      (max.x - min.x + 1L) * (max.y - min.y + 1L) * (max.z - min.z + 1L)
    }

    def subtract(other: Command): List[Command] = {
      intersection(other).map {
        sub =>
          val front = Option.when(sub.min.y > min.y)(copy(min = min, max = Point(max.x, sub.min.y - 1, max.z)))
          val back = Option.when(sub.max.y < max.y)(copy(min = Point(min.x, sub.max.y + 1, min.z), max = max))
          val bottom = Option.when(min.z < sub.min.z)(copy(min = Point(min.x, sub.min.y, min.z), max = Point(max.x, sub.max.y, sub.min.z - 1)))
          val top = Option.when(sub.max.z < max.z)(copy(min = Point(min.x, sub.min.y, sub.max.z + 1), max = Point(max.x, sub.max.y, max.z)))
          val left = Option.when(min.x < sub.min.x)(copy(min = Point(min.x, sub.min.y, sub.min.z), max = Point(sub.min.x - 1, sub.max.y, sub.max.z)))
          val right = Option.when(sub.max.x < max.x)(copy(min = Point(sub.max.x + 1, sub.min.y, sub.min.z), max = Point(max.x, sub.max.y, sub.max.z)))
          List(front, back, bottom, top, left, right).flatten
      }.getOrElse(this :: Nil)
    }

    private def intersection(other: Command): Option[Command] = {
      val minX = Math.max(min.x, other.min.x)
      val maxX = Math.min(max.x, other.max.x)
      if (maxX >= minX) {
        val minY = Math.max(min.y, other.min.y)
        val maxY = Math.min(max.y, other.max.y)
        if (maxY >= minY) {
          val minZ = Math.max(min.z, other.min.z)
          val maxZ = Math.min(max.z, other.max.z)
          if (maxZ >= minZ) {
            Some(copy(min = Point(minX, minY, minZ), max = Point(maxX, maxY, maxZ)))
          } else {
            None
          }
        } else {
          None
        }
      } else {
        None
      }
    }

    override def toString: String = {
      s"Command($min,$max,$size)"
    }

  }

  case class Point(x: Int, y: Int, z: Int)

}
