import java.nio.file.{Files, Path}

object Advent13 {

  def run(): Unit = {
    val lines = Files.readString(Path.of("./data/input13.txt")).linesIterator.filter(_.nonEmpty).toList
    val (pointLines, instructionLines) = lines.partition(_.contains(','))
    val points = pointLines.map(parsePoint).toSet
    val instructions = instructionLines.map(parseInstruction)
    val finalPoints = instructions.foldLeft(points)((set, instruction) => applyInstruction(set, instruction))
    visualizePoints(finalPoints) // EFJKZLBL
  }
  
  case class Instruction(axis: String, coordinate: Int)

  case class Point(x: Int, y: Int)

  private def visualizePoints(points: Set[Point]): Unit = {
    for {
      y <- 0 to points.map(_.y).max
    } yield {
      val outputLine = for {
        x <- 0 to points.map(_.x).max
      } yield {
        if (points.contains(Point(x, y))) '#' else '.'
      }
      println(outputLine.mkString)
    }
  }

  private def applyInstruction(points: Set[Point], instruction: Instruction): Set[Point] = {
    points.map {
      point =>
        instruction.axis match {
          case "x" if point.x > instruction.coordinate => point.copy(x = 2 * instruction.coordinate - point.x)
          case "y" if point.y > instruction.coordinate => point.copy(y = 2 * instruction.coordinate - point.y)
          case _ => point
        }
    }
  }

  private def parsePoint(line: String): Point = {
    val List(x, y) = line.split(',').toList.map(_.toInt)
    Point(x, y)
  }

  private def parseInstruction(line: String): Instruction = {
    val coordinatePart = line.split(' ').last
    val List(axis, coordinate) = coordinatePart.split('=').toList
    Instruction(axis, coordinate.toInt)
  }

}
