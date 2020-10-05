import org.scalatest.funsuite.AnyFunSuite

class PieceTest extends AnyFunSuite {

  test("An empty Set should have size 0") {
    val board: Board = Board(Set())

    board.printBoard()

    board.getPieceByPosition(Position(7, 3)).pieceColor

    assert(board.getPieceByPosition(Position(7, 3)).pieceType.equals(King))
    assert(board.isEmptyField(Position(3, 3)))
    assert(board.isEnemyPiece(Position(7, 3), White))
  }

}