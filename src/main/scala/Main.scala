import scala.annotation.tailrec

def placeNQueens(initialBoard: Board, n: Int): Set[Board] =

  @tailrec
  def loop(acc: Set[Board], n: Int): Set[Board] =
    if (n == 0) acc
    else
      val nextBoards = acc.filter(_.canPlaceQueen ).flatMap { board =>
        board.unthreatenedPositions.map(board.placeQueenAt)
      }
      loop(nextBoards, n - 1)

  loop(Set(initialBoard), n).filter(_.queens.size == n)

@main def run(): Unit = {
  val eightQueens = placeNQueens(Board.empty, 8)
  eightQueens.takeRight(5).foreach(board => println(board.show))
  println(eightQueens.size)
}
