import org.scalatest.funsuite.AnyFunSuite

class MoveDirectionPositionsTest extends AnyFunSuite {

  test("") {
    var board: Board = Board(Set())

    board.printBoard()

    val position: Position = Position(1, 0)
    board.getPieceByPosition(position).getPossibleMoves(board, position)
  }

}