import java.nio.file.{Files, Path}

object Advent12 {

  def run(): Unit = {

    val lines = Files.readString(Path.of("./data/input12.txt")).linesIterator.toSeq

    val map = lines.flatMap {
      line =>
        val List(start, end) = line.split('-').toList
        List((start, end), (end, start))
    }.groupBy(_._1).map {
      case (start, list) => (start, list.map(_._2).toList)
    }

    val paths = getPaths(map)
    println(paths.length)
  }

  private def getPaths(map: Map[String, List[String]],
                       path: List[String] = "start" :: Nil,
                       visited: Set[String] = Set("start"),
                       visitedSmall: Option[String] = None
                      ): List[List[String]] = {
    map(path.head).flatMap {
      destination =>
        if (destination == "end") {
          (destination :: path) :: Nil
        } else if (destination == "start") {
          Nil // never visit start again
        } else if (visited.contains(destination) && visitedSmall.isDefined) {
          Nil // never visit small caves again, unless not visited twice
        } else {
          if (destination.head.isLower) {
            if (visited.contains(destination)) {
              getPaths(map, destination :: path, visited, visitedSmall = Some(destination))
            } else {
              getPaths(map, destination :: path, visited + destination, visitedSmall)
            }
          } else {
            getPaths(map, destination :: path, visited, visitedSmall)
          }
        }
    }
  }

}
