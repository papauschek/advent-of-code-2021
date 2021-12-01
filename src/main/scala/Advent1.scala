import java.nio.file.{Files, Path}

object Advent1 {

  def run(): Unit = {
    val file = Files.readString(Path.of("./data/input1.txt"))
    val measurements = file.linesIterator.map(_.toInt).toList
    val sliding = measurements.sliding(3).toList
    val increasedCount = sliding.zip(sliding.tail).count {
      case (previous, current) => previous.sum < current.sum
    }
    println(increasedCount)
  }

}
