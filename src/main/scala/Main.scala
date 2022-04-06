def unthreatenedPositions(board: Board): Set[Position] =
  Board.allPositions diff board.allThreatened

def canPlaceQueen(board: Board): Boolean =
  unthreatenedPositions(board).nonEmpty

@main def run(): Unit =
  val position = Position(Rank.fromInt(4).get, File.D)
  val queen = Queen(position)
  val board = Board.empty.placeQueen(queen)
  val nextQueen = Queen(unthreatenedPositions(board).head)
  println(board.placeQueen(nextQueen).show)
