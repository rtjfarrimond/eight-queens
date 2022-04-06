// For more information on writing tests, see
// https://scalameta.org/munit/docs/getting-started.html
class MySuite extends munit.FunSuite {

  test("Bottom left corner queen threatens correct positions") {
    val rank = Rank.fromInt(1).get
    val file = File.A
    val position = Position(rank, file)
    val queen = Queen(position)

    val rowThreatened = File.values.tail.map(f => Position(rank, f)).toSet
    val columnThreatened = Rank.allRanks.tail.map(r => Position(r, file)).toSet
    val diagThreatened = Set(
      Position(Rank.fromInt(2).get, File.B),
      Position(Rank.fromInt(3).get, File.C),
      Position(Rank.fromInt(4).get, File.D),
      Position(Rank.fromInt(5).get, File.E),
      Position(Rank.fromInt(6).get, File.F),
      Position(Rank.fromInt(7).get, File.G),
      Position(Rank.fromInt(8).get, File.H)
    )

    val expected = rowThreatened ++ columnThreatened ++ diagThreatened

    assertEquals(queen.threatens, expected)
  }

}
