
object Advent21 {

  def main(args: Array[String]): Unit = {

    var universes = List(Universe(Game(), count = 1))

    while (universes.exists(!_.game.hasWinner)) {
      universes = grouped(universes.flatMap {
        universe =>
          if (universe.game.hasWinner) {
            universe :: Nil
          } else {
            playHalfRound(universe.game).map(game => universe.copy(game = game))
          }
      })
    }

    val (firstWinner, secondWinner) = universes.partition(_.game.players.head.hasWon)
    println(firstWinner.map(_.count).sum, secondWinner.map(_.count).sum)
  }

  private def grouped(universes: List[Universe]): List[Universe] = {
    universes.groupBy(_.game).map {
      case (game, list) => Universe(game, count = list.map(_.count).sum)
    }.toList
  }

  private def playHalfRound(game: Game): Seq[Game] = {
    val currentPlayer = game.players(game.currentPlayerIndex)
    val nextPlayerIndex = (game.currentPlayerIndex + 1) % 2
    playRound(currentPlayer).map {
      nextPlayer =>
        val nextPlayers =
          if (game.currentPlayerIndex == 0) {
            nextPlayer :: game.players(1) :: Nil
          } else {
            game.players.head :: nextPlayer :: Nil
          }
        Game(nextPlayers, nextPlayerIndex)
    }
  }

  private def playRound(player: Player): Seq[Player] = {
    for {
      dice1 <- 1 to 3
      dice2 <- 1 to 3
      dice3 <- 1 to 3
    } yield {
      val total = dice1 + dice2 + dice3
      val position = (player.position + total - 1) % 10 + 1
      player.copy(
        position = position,
        score = player.score + position
      )
    }
  }

  case class Universe(game: Game, count: Long)

  case class Game(players: List[Player] = List(Player(6), Player(8)),
                  currentPlayerIndex: Int = 0) {

    def hasWinner: Boolean = players.exists(_.hasWon)

  }

  case class Player(position: Int, score: Int = 0) {

    def hasWon: Boolean = score >= 21

  }

}
