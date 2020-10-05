import org.scalatest.funsuite.AnyFunSuite

class BoardTest extends AnyFunSuite {

  test("board test") {
    val board: Board = Board(Set())

    board.printBoard()

    assert(board.getPieceByPosition(Position(3, 7)).pieceType.equals(King))
    assert(board.isEmptyField(Position(3, 3)))
    assert(board.isEnemyPiece(Position(3, 7), White))

    assert(board.getKingPosition(White).equals(Position(4, 0)))
    assert(board.getKingPosition(Black).equals(Position(3, 7)))
  }

}