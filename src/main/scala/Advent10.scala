import java.nio.file.{Files, Path}
import scala.annotation.tailrec

object Advent10 {

  def run(): Unit = {
    val lines = Files.readString(Path.of("./data/input10.txt")).linesIterator.toList
    val scores = lines.map(line => scoreLine(line)).filter(_ > 0)
    val median = scores.sorted.apply(scores.length / 2)
    println(median) // 1118645287
  }

  val brackets: Map[Char, Char] = Map(
    '(' -> ')',
    '[' -> ']',
    '{' -> '}',
    '<' -> '>'
  )

  val score: Map[Char, Int] = Map(
    ')' -> 1,
    ']' -> 2,
    '}' -> 3,
    '>' -> 4
  )

  @tailrec
  private def scoreLine(line: String, openBrackets: List[Char] = Nil): Long = {
    if (line.isEmpty) {
      openBrackets.foldLeft(0L)((total, char) => total * 5 + score(char))
    } else {
      val char = line.head
      if (brackets.contains(char)) {
        scoreLine(line.tail, brackets(char) :: openBrackets)
      } else {
        openBrackets.headOption match {
          case Some(closeChar) if closeChar == char =>
            scoreLine(line.tail, openBrackets.tail)
          case _ => 0
        }
      }
    }
  }



}
