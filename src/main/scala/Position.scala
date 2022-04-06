import cats.syntax.all._

case class Position(rank: Rank, file: File)
extension (p: Position)
  def northEast: Option[Position] =
    (p.rank.successor, p.file.successor).mapN(Position.apply)
  def southEast: Option[Position] =
    (p.rank.predecessor, p.file.successor).mapN(Position.apply)
  def southWest: Option[Position] =
    (p.rank.predecessor, p.file.predecessor).mapN(Position.apply)
  def northWest: Option[Position] =
    (p.rank.successor, p.file.predecessor).mapN(Position.apply)
  def diagonals: Set[Position] =
    val neDiags = Set.unfold(p)(pos => (pos.northEast.zip(pos.northEast)))
    val seDiags = Set.unfold(p)(pos => (pos.southEast.zip(pos.southEast)))
    val swDiags = Set.unfold(p)(pos => (pos.southWest.zip(pos.southWest)))
    val nwDiags = Set.unfold(p)(pos => (pos.northWest.zip(pos.northWest)))
    neDiags ++ seDiags ++ swDiags ++ nwDiags

