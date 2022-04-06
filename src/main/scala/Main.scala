import cats.syntax.all._

enum File(val value: Int):
  case A extends File(1)
  case B extends File(2)
  case C extends File(3)
  case D extends File(4)
  case E extends File(5)
  case F extends File(6)
  case G extends File(7)
  case H extends File(8)

extension (f: File)
  def predecessor: Option[File] =
    val values = File.values.toList
    val index = values.indexOf(f)
    if (index == 0) None
    else Some(values(index - 1))

  def successor: Option[File] =
    val values = File.values.toList
    val index = values.indexOf(f)
    if (index == values.length -1) None
    else Some(values(index + 1))

opaque type Rank = Int
object Rank:
  val allRanks: List[Rank] = (1 to 8).map(identity).toList
  def fromInt(i: Int): Option[Rank] =
    if (allRanks.contains(i)) Some(i) else None
extension(r: Rank)
  def predecessor: Option[Rank] =
    Rank.fromInt(r - 1)
  def successor: Option[Rank] =
    Rank.fromInt(r + 1)

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

case class Board(queens: Set[Queen])
object Board:
  val allPositions = (Rank.allRanks, File.values.toList).mapN(Position.apply)

// case class State(board: Board, valid: Set[Board])
// def run(): Set[Board] =
//   def placeQueen(board: board): Board =


@main def hello: Unit =
  val rank = Rank.fromInt(4).get
  val file = File.D
  val pos = Position(rank, file)
  println(pos.diagonals)
  println(Queen(pos).threatens)
