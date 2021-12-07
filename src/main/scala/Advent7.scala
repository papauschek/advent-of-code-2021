import java.nio.file.{Files, Path}

object Advent7 {

  def run(): Unit = {
    val file = Files.readString(Path.of("./data/input7.txt"))
    val positions = file.split(',').toList.map(_.toInt)
    val minPosition = (positions.sum / positions.length.toDouble).toInt
    println(getFuelCost(positions, minPosition)) // 95581659
  }

  private def getFuelCost(positions: Seq[Int], targetPosition: Int): Int = {
    positions.map {
      p =>
        val steps = (p - targetPosition).abs
        ((steps + 1) * steps) / 2
    }.sum
  }

}
