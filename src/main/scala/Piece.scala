import java.util.Optional

import MoveDirection.MoveDirection
import MoveType.MoveType

abstract class PieceColor
  case object White extends PieceColor
  case object Black extends PieceColor

abstract class PieceType
  case object Pawn extends PieceType
  case object Rook extends PieceType
  case object Knight extends PieceType
  case object Bishop extends PieceType
  case object King extends PieceType
  case object Queen extends PieceType

object MoveBoundaries {
  val minX = 0
  val maxX = 7
  val minY = 0
  val maxY = 7
}

object MoveType extends Enumeration {
  type MoveType = Value
  val ATTACK, MOVE, ATTACK_AND_MOVE = Value
}

object MoveDirection extends Enumeration {
  type MoveDirection = Value
  val TOP, TOP_BY_2,
      BOTTOM, BOTTOM_BY_2,
      LEFT, RIGHT,
      TOP_LEFT, TOP_RIGHT,
      BOTTOM_LEFT, BOTTOM_RIGHT,
      L_SHAPE,
      HORIZONTALLY, VERTICALLY, DIAGONAL,
      CASTLING = Value
}

object GetMovePositions {

  def top(position: Position): Position = Position(position.x, position.y + 1)
  def bottom(position: Position): Position = Position(position.x, position.y - 1)
  def topBy2(position: Position): Position = Position(position.x, position.y + 2)
  def bottomBy2(position: Position): Position = Position(position.x, position.y - 2)
  def left(position: Position): Position = Position(position.x - 1, position.y)
  def right(position: Position): Position = Position(position.x + 1, position.y)
  def topLeft(position: Position): Position = Position(position.x - 1, position.y + 1)
  def topRight(position: Position): Position = Position(position.x + 1, position.y + 1)
  def bottomLeft(position: Position): Position = Position(position.x - 1, position.y - 1)
  def bottomRight(position: Position): Position = Position(position.x + 1, position.y - 1)
  def diagonallyLeftUp(position: Position): Position = Position(position.x - 1, position.y + 1)
  def diagonallyRightUp(position: Position): Position = Position(position.x + 1, position.y + 1)
  def diagonallyRightDown(position: Position): Position = Position(position.x + 1, position.y - 1)
  def diagonallyLeftDown(position: Position): Position = Position(position.x - 1, position.y - 1)

  def lShape(position: Position): List[Position] = {
    List(Position(position.x + 1, position.y + 2),
      Position(position.x - 1, position.y + 2),
      Position(position.x + 1, position.y - 2),
      Position(position.x - 1, position.y - 2),
      Position(position.x + 2, position.y + 1),
      Position(position.x + 2, position.y - 1),
      Position(position.x - 2, position.y + 1),
      Position(position.x - 2, position.y - 1))
  }

  def kingMoves(position: Position): List[Position] = {
    List(top(position),
      bottom(position),
      bottomRight(position),
      bottomLeft(position),
      topRight(position),
      topLeft(position))
  }

}

case class MoveDirectionPositions() {

  var movePosition: Set[Position] = Set.empty
  var attackPosition: Set[Position] = Set.empty

  def getMoveDirectionPositions(position: Position, move: Move, board: Board, pieceColor: PieceColor): Unit = {
    move.moveDirection match {
      case MoveDirection.TOP => explore(move, board, GetMovePositions.top(position), pieceColor)
      case MoveDirection.TOP_BY_2 => explore(move, board, GetMovePositions.topBy2(position), pieceColor)
      case MoveDirection.BOTTOM => explore(move, board, GetMovePositions.bottom(position), pieceColor)
      case MoveDirection.BOTTOM_BY_2 => explore(move, board, GetMovePositions.bottomBy2(position), pieceColor)
      case MoveDirection.LEFT => explore(move, board, GetMovePositions.left(position), pieceColor)
      case MoveDirection.RIGHT => explore(move, board, GetMovePositions.right(position), pieceColor)
      case MoveDirection.TOP_LEFT => explore(move, board, GetMovePositions.topLeft(position), pieceColor)
      case MoveDirection.TOP_RIGHT => explore(move, board, GetMovePositions.topRight(position), pieceColor)
      case MoveDirection.BOTTOM_LEFT => explore(move, board, GetMovePositions.bottomLeft(position), pieceColor)
      case MoveDirection.BOTTOM_RIGHT => explore(move, board, GetMovePositions.bottomRight(position), pieceColor)
      case MoveDirection.L_SHAPE => GetMovePositions.lShape(position).foreach(p => explore(move, board, p, pieceColor))
      case MoveDirection.HORIZONTALLY => exploreHorizontallyPositions(position, move, board, pieceColor)
      case MoveDirection.VERTICALLY => exploreVerticallyPositions(position, move, board, pieceColor)
      case MoveDirection.DIAGONAL => exploreDiagonalPositions(position, move, board, pieceColor)
      case MoveDirection.CASTLING =>
    }
  }

  def exploreDiagonalPositions(position: Position, move: Move, board: Board, pieceColor: PieceColor) = {
    val topLeft = (position: Position) => Position(position.x - 1, position.y + 1)
    val topRight = (position: Position) => Position(position.x + 1, position.y + 1)
    val bottomLeft = (position: Position) => Position(position.x - 1, position.y - 1)
    val bottomRight = (position: Position) => Position(position.x + 1, position.y - 1)

    for (w <- 0 to 3) {
      var continue: Boolean = true
      var pos: Position = position
      while (continue) {
        w match {
          case 0 => pos = topLeft(position)
          case 1 => pos = topRight(position)
          case 2 => pos = bottomLeft(position)
          case 3 => pos = bottomRight(position)
        }
        if (inBoundaries(pos)) {
          continue = explore(move, board, pos, pieceColor)
        } else {
          continue = false
        }
      }
    }
  }

  def inBoundaries(position: Position): Boolean = {
    position.x <= MoveBoundaries.maxX && position.x >= MoveBoundaries.minX &&
      position.y <= MoveBoundaries.maxY && position.y >= MoveBoundaries.minY
  }

  def exploreVerticallyPositions(position: Position, move: Move, board: Board, pieceColor: PieceColor) = {
    var posY = position.y
    var continue: Boolean = true
    while (continue && posY < MoveBoundaries.maxY) {
      posY += 1
      continue = explore(move, board, Position(position.x, posY), pieceColor)
    }
    posY = position.y
    continue = true
    while (continue && posY > MoveBoundaries.minY) {
      posY -= 1
      continue = explore(move, board, Position(position.x, posY), pieceColor)
    }
  }

  def exploreHorizontallyPositions(position: Position, move: Move, board: Board, pieceColor: PieceColor) = {
    var posX = position.x
    var continue: Boolean = true
    while (continue && posX < MoveBoundaries.maxX) {
      posX += 1
      continue = explore(move, board, Position(posX, position.y), pieceColor)
    }
    posX = position.x
    continue = true
    while (continue && posX > MoveBoundaries.minX) {
      posX -= 1
      continue = explore(move, board, Position(posX, position.y), pieceColor)
    }
  }

  case class ExploreMoveResult(attackPosition: Position, movePosition: Position, continue: Boolean)

  def explore(move: Move, board: Board, position: Position, pieceColor: PieceColor): Boolean = {
    if (!inBoundaries(position) || board.isMyPiece(position, pieceColor))
      return false

    val exploreMoveResult: ExploreMoveResult = exploreMove(move, board, position, pieceColor)
    Optional.ofNullable(exploreMoveResult.attackPosition).ifPresent(pos => this.attackPosition += pos)
    Optional.ofNullable(exploreMoveResult.movePosition).ifPresent(pos => this.movePosition += pos)
    exploreMoveResult.continue
  }

  def exploreMove(move: Move, board: Board, pos: Position, pieceColor: PieceColor): ExploreMoveResult = {
    var attackPosition: Position = null
    var movePosition: Position = null
    var continue: Boolean = false
    move.moveType match {
      case MoveType.ATTACK =>
        if (board.isEnemyPiece(pos, pieceColor)) {
          attackPosition = pos
        }
      case MoveType.MOVE =>
        if (board.isEmptyField(pos)) {
          movePosition = pos
        } else {
          continue = false
        }
      case MoveType.ATTACK_AND_MOVE =>
        if (board.isEnemyPiece(pos, pieceColor)) {
          attackPosition = pos
          continue = false
        } else {
          movePosition = pos
        }
    }
    ExploreMoveResult(attackPosition, movePosition, continue)
  }
}

case class Move(moveType: MoveType, moveDirection: MoveDirection)

object MoveProcessor {
  def getMovePositions(position: Position, board: Board): List[Position] = {
    board.getPieceByPosition(position).moves

    List.empty
  }

  def getAttackPositions(): List[Position] = {
    List.empty
  }
}

case class Position(x: Int, y: Int)

case class Piece(pieceColor: PieceColor, pieceType: PieceType) {

  lazy val moves = pieceType match {
    case Pawn    => pieceColor match {
      case White => List[Move](
        Move(MoveType.ATTACK, MoveDirection.TOP_RIGHT),
        Move(MoveType.ATTACK, MoveDirection.TOP_LEFT),
        Move(MoveType.MOVE, MoveDirection.TOP),
        Move(MoveType.MOVE, MoveDirection.TOP_BY_2))
      case Black => List[Move](
        Move(MoveType.ATTACK, MoveDirection.BOTTOM_RIGHT),
        Move(MoveType.ATTACK, MoveDirection.BOTTOM_LEFT),
        Move(MoveType.MOVE, MoveDirection.BOTTOM),
        Move(MoveType.MOVE, MoveDirection.BOTTOM_BY_2))
    }
    case Knight  => List[Move](Move(MoveType.ATTACK_AND_MOVE, MoveDirection.L_SHAPE))
    case Bishop  => List[Move](Move(MoveType.ATTACK_AND_MOVE, MoveDirection.DIAGONAL))
    case Rook    => List[Move](
      Move(MoveType.ATTACK_AND_MOVE, MoveDirection.HORIZONTALLY),
      Move(MoveType.ATTACK_AND_MOVE, MoveDirection.VERTICALLY))
    case Queen   => List[Move](
      Move(MoveType.ATTACK_AND_MOVE, MoveDirection.HORIZONTALLY),
      Move(MoveType.ATTACK_AND_MOVE, MoveDirection.VERTICALLY),
      Move(MoveType.ATTACK_AND_MOVE, MoveDirection.DIAGONAL))
    case King    => List[Move](
      Move(MoveType.ATTACK_AND_MOVE, MoveDirection.TOP),
      Move(MoveType.ATTACK_AND_MOVE, MoveDirection.TOP_LEFT),
      Move(MoveType.ATTACK_AND_MOVE, MoveDirection.TOP_RIGHT),
      Move(MoveType.ATTACK_AND_MOVE, MoveDirection.BOTTOM),
      Move(MoveType.ATTACK_AND_MOVE, MoveDirection.BOTTOM_LEFT),
      Move(MoveType.ATTACK_AND_MOVE, MoveDirection.BOTTOM_RIGHT),
      Move(MoveType.MOVE, MoveDirection.CASTLING))
  }

  def getPossibleMoves(board: Board, position: Position): List[Position] = {
    var moveDirectionPositions: MoveDirectionPositions = MoveDirectionPositions()
    moves.foreach(move => {
      moveDirectionPositions.getMoveDirectionPositions(position, move, board, pieceColor)
    })
    moveDirectionPositions.movePosition.foreach(p => println("m: " + p))
    moveDirectionPositions.attackPosition.foreach(p => println("a: " + p))

    List.empty
  }

}
