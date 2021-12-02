import java.nio.file.{Files, Path}

object Advent2 {

  def run(): Unit = {
    val commands = Files.readString(Path.of("./data/input2.txt")).linesIterator.toList
    val firstPosition = Position(horizontal = 0, depth = 0, aim = 0)
    val lastPosition = commands.foldLeft(firstPosition)((position, command) => position.move(command))
    println(lastPosition.horizontal * lastPosition.depth)
  }

}

case class Position(horizontal: Int, depth: Int, aim: Int) {

  def move(command: String): Position = {
    val parts = command.split(' ')
    val distance = parts.last.toInt
    parts.head match {
      case "forward" => copy(horizontal = horizontal + distance, depth = depth + distance * aim)
      case "down" => copy(aim = aim + distance)
      case "up" => copy(aim = aim - distance)
    }
  }

}