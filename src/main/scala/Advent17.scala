
object Advent17 {

  def main(args: Array[String]): Unit = {

    val targetTopLeft = Point(175, -79)
    val targetBottomRight = Point(227, -134)

    def simulateShot(initialVelocity: Point): Boolean = {
      var velocity = initialVelocity
      var position = Point(0, 0)
      var maxY = position.y
      var found = false
      while (!found && position.x <= targetBottomRight.x && position.y >= targetBottomRight.y) {
        if (position.x >= targetTopLeft.x && position.y <= targetTopLeft.y) {
          found = true
        } else {
          position = Point(position.x + velocity.x, position.y + velocity.y)
          velocity = Point(velocity.x - velocity.x.sign, velocity.y - 1)
          maxY = Math.max(maxY, position.y)
        }
      }
      found
    }

    val shots = for {
      x <- 1 to 1000
      y <- -1000 to 1000
      shot = Point(x, y) if simulateShot(shot)
    } yield shot

    println(shots.length)

  }



  case class Point(x: Int, y: Int)

}
