import java.nio.file.{Files, Path}

object Advent6 {

  def run(): Unit = {
    val file = Files.readString(Path.of("./data/input6.txt"))
    val initialGroups = file.split(',').toList.map(timer => Group(timer.toInt, count = 1))
    val finalGroups = (0 until 256).foldLeft(initialGroups)((groups, _) => iteration(groups))
    println(finalGroups.map(_.count).sum) // 1572358335990
  }

  private def iteration(groups: List[Group]): List[Group] = {
    group(groups).flatMap {
      group =>
        group.timer match {
          case 0     => Group(timer = 6, group.count) :: Group(timer = 8, group.count) :: Nil
          case timer => group.copy(timer = timer - 1) :: Nil
        }
    }
  }

  private def group(groups: List[Group]): List[Group] = {
    groups.groupBy(_.timer).toList.map { case (timer, list) => Group(timer, list.map(_.count).sum) }
  }

  case class Group(timer: Int, count: Long)

}
