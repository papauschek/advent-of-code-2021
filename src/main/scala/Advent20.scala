import java.nio.file.{Files, Path}

object Advent20 {

  def main(args: Array[String]): Unit = {

    val List(algorithmLine, imageLines) = Files.readString(Path.of("./data/input20.txt")).split("\n\n").toList

    val algorithm = algorithmLine.zipWithIndex.flatMap {
      case (char, index) => Some(index).filter(_ => char == '#')
    }.toSet

    var image = Image(imageLines.linesIterator.toVector, default = '.')
    for {
      index <- 0 until 50
    } yield {
      image = enhance(image, algorithm)
      println(index, image.litPixels)
    }
  }

  private def enhance(image: Image, algorithm: Set[Int]): Image = {
    val lines = for {
      y <- -1 to image.lines.length
    } yield {
      (for {
        x <- -1 to image.lines.head.length
      } yield enhancePixel(x, y, image, algorithm)).mkString
    }
    val defaultInput = if (image.default == '.') 0 else 511
    Image(lines.toVector, default = if (algorithm(defaultInput)) '#' else '.')
  }

  private def enhancePixel(x: Int, y: Int, image: Image, algorithm: Set[Int]): Char = {
    var number = 0
    for {
      dy <- -1 to 1
      dx <- -1 to 1
    } yield {

      val char: Char = (for {
        line <- image.lines.lift.apply(y + dy)
        char <- line.lift.apply(x + dx)
      } yield char).getOrElse(image.default)

      if (char == '#') {
        val bit = (dy + 1) * 3 + dx + 1
        number |= 1 << (8 - bit)
      }
    }

    if (algorithm.contains(number)) '#' else '.'
  }

  case class Point(x: Int, y: Int)

  case class Image(lines: Vector[String], default: Char) {

    def litPixels: Long = {
      lines.map(_.count(_ == '#').toLong).sum
    }

    def print(): Unit = {
      println()
      println(litPixels, default)
      println(lines.mkString("\r\n"))
    }

  }

}
