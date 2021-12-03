import java.nio.file.{Files, Path}

object Advent3 {

  def run(): Unit = {
    val numbers = Files.readString(Path.of("./data/input3.txt")).linesIterator.toList
    val oxygenRate = getRate(numbers, oxygen = true)
    val scrubberRate = getRate(numbers, oxygen = false)
    println(oxygenRate * scrubberRate)
  }

  private def getRate(numbers: List[String], oxygen: Boolean, index: Int = 0): Int = {
    if (numbers.length == 1) {
      Integer.parseInt(numbers.head, 2)
    } else {
      val (ones, zeros) = numbers.partition(_.apply(index) == '1')
      if ((ones.length >= zeros.length) == oxygen) {
        getRate(ones, oxygen, index + 1)
      } else {
        getRate(zeros, oxygen, index + 1)
      }
    }
  }

}
