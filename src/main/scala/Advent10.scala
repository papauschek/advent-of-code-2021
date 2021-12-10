import java.nio.file.{Files, Path}
import scala.annotation.tailrec

object Advent10 {

  def run(): Unit = {
    val lines = Files.readString(Path.of("./data/input10.txt")).linesIterator.toList
    val scores = lines.map(line => scoreLine(line))
    println(scores.sum) // 366027
  }

  val brackets: Map[Char, Char] = Map(
    '(' -> ')',
    '[' -> ']',
    '{' -> '}',
    '<' -> '>'
  )

  val score: Map[Char, Int] = Map(
    ')' -> 3,
    ']' -> 57,
    '}' -> 1197,
    '>' -> 25137
  )

  @tailrec
  private def scoreLine(line: String, openBrackets: List[Char] = Nil): Int = {
    if (line.isEmpty) {
      0
    } else {
      val char = line.head
      if (brackets.contains(char)) {
        scoreLine(line.tail, brackets(char) :: openBrackets)
      } else {
        openBrackets.headOption match {
          case Some(closeChar) =>
            if (closeChar == char) {
              scoreLine(line.tail, openBrackets.tail)
            } else {
              score(char)
            }
          case _ => 0
        }
      }
    }
  }



}
