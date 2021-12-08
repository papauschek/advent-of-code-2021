import java.nio.file.{Files, Path}

object Advent8 {

  val ALL_CHARS: String = "abcdefg"

  val DIGITS: Map[String, Char] = Map(
    "abcefg" -> '0',
    "cf" -> '1',
    "acdeg" -> '2',
    "acdfg" -> '3',
    "bcdf" -> '4',
    "abdfg" -> '5',
    "abdefg" -> '6',
    "acf" -> '7',
    "abcdefg" -> '8',
    "abcdfg" -> '9'
  )

  def run(): Unit = {
    val lines = Files.readString(Path.of("./data/input8.txt")).linesIterator.toList
    val patterns = lines.map(_.split('|').toList.map(_.trim.split(' ').toList))
    val decoders = getDecoders(ALL_CHARS)
    val sum = patterns.map(pairs => getOutput(decoders, pairs)).sum
    println(sum)
  }

  private def getOutput(decoders: List[String], pairs: List[List[String]]): Int = {
    val decoder = getDecoder(decoders, pairs.flatten)
    val decoded = pairs.last.map(code => DIGITS(decode(decoder, code)))
    decoded.mkString.toInt
  }

  private def getDecoder(decoders: List[String], patterns: Seq[String]): String = {
    decoders.find {
      decoder =>
        patterns.forall {
          pattern => DIGITS.contains(decode(decoder, pattern))
        }
    }.get
  }

  private def decode(decoder: String, pattern: String): String = {
    pattern.map {
      char => decoder(ALL_CHARS.indexOf(char))
    }.sorted
  }

  private def getDecoders(chars: String, prefix: String = ""): List[String] = {
    if (chars.isEmpty) {
      prefix :: Nil
    } else {
      chars.flatMap {
        char => getDecoders(chars.filterNot(_ == char), prefix + char)
      }.toList
    }
  }

}
