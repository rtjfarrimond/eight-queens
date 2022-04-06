class BoardSuite extends munit.FunSuite {

  test("The empty board must show no queens") {
    val expected = """
    |   _______________
    |8 |_|_|_|_|_|_|_|_|
    |7 |_|_|_|_|_|_|_|_|
    |6 |_|_|_|_|_|_|_|_|
    |5 |_|_|_|_|_|_|_|_|
    |4 |_|_|_|_|_|_|_|_|
    |3 |_|_|_|_|_|_|_|_|
    |2 |_|_|_|_|_|_|_|_|
    |1 |_|_|_|_|_|_|_|_|
    |   a b c d e f g h""".stripMargin
    assertEquals(Board.empty.show,  expected)
  }

  test("Must show a single queen at D4") {
    val position = Position(Rank.fromInt(4).get, File.D)
    val queen = Queen(position)
    val board = Board(Set(queen))
    val expected = """
    |   _______________
    |8 |_|_|_|.|_|_|_|.|
    |7 |.|_|_|.|_|_|.|_|
    |6 |_|.|_|.|_|.|_|_|
    |5 |_|_|.|.|.|_|_|_|
    |4 |.|.|.|Q|.|.|.|.|
    |3 |_|_|.|.|.|_|_|_|
    |2 |_|.|_|.|_|.|_|_|
    |1 |.|_|_|.|_|_|.|_|
    |   a b c d e f g h""".stripMargin
    assertEquals(board.show,  expected)
  }

  test("Must show a queen at D4 and another at H6") {
    val d4 = Position(Rank.fromInt(4).get, File.D)
    val queenD4 = Queen(d4)
    val h6 = Position(Rank.fromInt(6).get, File.H)
    val queenH6 = Queen(h6)
    val board = Board(Set(queenD4, queenH6))
    val expected = """
    |   _______________
    |8 |_|_|_|.|_|.|_|.|
    |7 |.|_|_|.|_|_|.|.|
    |6 |.|.|.|.|.|.|.|Q|
    |5 |_|_|.|.|.|_|.|.|
    |4 |.|.|.|Q|.|.|.|.|
    |3 |_|_|.|.|.|_|_|.|
    |2 |_|.|_|.|_|.|_|.|
    |1 |.|_|.|.|_|_|.|.|
    |   a b c d e f g h""".stripMargin
    assertEquals(board.show,  expected)
  }

}
