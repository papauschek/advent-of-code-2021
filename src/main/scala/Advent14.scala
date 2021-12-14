import java.nio.file.{Files, Path}

object Advent14 {

  def run(): Unit = {
    val lines = Files.readString(Path.of("./data/input14.txt")).linesIterator.filter(_.nonEmpty).toList
    val initialPolymer = lines.head
    val pairs = initialPolymer.sliding(2).toList.map(pair => Pair(pair, count = 1))
    val rules = lines.tail.map(parseRule).toMap
    val polymer = (0 until 40).foldLeft(pairs)((pairs, _) => applyRules(pairs, rules))
    val histogram = polymer.groupBy(_.pair.head).toList.map { case (_, list) => list.map(_.count).sum }
    println(histogram.max - histogram.min + 1) // extra char at the end which was ignored during processing
  }

  private def applyRules(pairs: List[Pair], rules: Map[String, String]): List[Pair] = {
    pairs.flatMap {
      pair =>
        rules.get(pair.pair) match {
          case Some(insert) => Pair(pair.pair(0) + insert, pair.count) :: Pair(insert + pair.pair(1), pair.count) :: Nil
          case _ => pair :: Nil
        }
    }.groupBy(_.pair).toList.map {
      case (pair, list) => Pair(pair, list.map(_.count).sum)
    }
  }

  private def parseRule(line: String): (String, String) = {
    val List(template, insert) = line.split(" -> ").toList
    (template, insert)
  }

  case class Pair(pair: String, count: Long)

}
