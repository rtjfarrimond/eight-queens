case class Queen(position: Position)

extension (q: Queen)
  def threatens: Set[Position] =
    val horizontals = Rank.allRanks.map { rank =>
      Position(rank, q.position.file)
    }
    val verticals = File.values.toList.map { file =>
      Position(q.position.rank, file)
    }
    val diagonals = q.position.diagonals
    (horizontals.toSet ++ verticals.toSet ++ diagonals) - q.position

