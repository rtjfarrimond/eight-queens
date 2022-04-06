import cats.syntax.all._

case class Board(queens: Set[Queen])
object Board:
  val empty: Board = Board(Set.empty[Queen])
  val allPositions: Set[Position] = (Rank.allRanks, File.allFiles).mapN(Position.apply).toSet

  extension (board: Board)
    def allThreatened: Set[Position] =
      board.queens.foldLeft(Set.empty[Position]) {
        case (acc, queen) => acc ++ queen.threatens
      }

    def placeQueen(queen: Queen): Board =
      board.copy(queens = board.queens + queen)

    def show: String =
      val firstLine = "\n   _______________\n"
      val builder = new StringBuilder(firstLine)

      Rank.allRanks.reverse.foreach { rank =>
        builder ++= s"$rank "
        val queensOnRank = board.queens.filter(_.position.rank == rank)
        val rankStrings = File.allFiles.map { file =>
          val currentPosition = Position(rank, file)
          if (queensOnRank.map(_.position).contains(currentPosition)) "|Q"
          else if (board.allThreatened.contains(currentPosition)) "|."
          else "|_"
        }
        rankStrings.foreach(s => builder ++= s)
        builder ++= s"|\n"
      }

      val fileLabels = "   a b c d e f g h"
      builder ++= fileLabels
      builder.toString
