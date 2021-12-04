import java.nio.file.{Files, Path}

object Advent4 {

  def run(): Unit = {
    val file = Files.readString(Path.of("./data/input4.txt")).linesIterator.toList
    val randomNumbers = file.head.split(',').toList
    val boardLines = file.tail

    var boards = for {
      index <- 0 until boardLines.length / 6
    } yield {
      val lines = boardLines.slice(index * 6 + 1, index * 6 + 6)
      Board(lines.map(line => Line(line.split(' ').toList.filter(_.nonEmpty))))
    }

    var lastBoardIndex = 0

    val losingNumber = randomNumbers.find {
      number =>
        lastBoardIndex = boards.indexWhere(!_.hasWon)
        boards = boards.map(_.remove(number))
        boards.forall(_.hasWon)
    }.get.toInt

    val lastBoard = boards(lastBoardIndex)

    println(losingNumber * lastBoard.sum)
  }

  case class Board(lines: Seq[Line]) {

    def remove(number: String): Board = {
      copy(
        lines = lines.map(line => line.copy(
          columns = line.columns.map(column => if (column == number) "" else column)
        ))
      )
    }

    def hasWon: Boolean = {
      val rowWon = lines.exists(_.columns.forall(_.isEmpty))
      val columnWon = lines.indices.exists(column => lines.forall(line => line.columns(column).isEmpty))
      rowWon || columnWon
    }

    def sum: Int = {
      lines.map(_.columns.flatMap(_.toIntOption).sum).sum
    }

  }

  case class Line(columns: Seq[String])

}
