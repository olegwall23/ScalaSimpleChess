
case class Board(pieces: Set[BoardPieces]) {

  val board: Array[Array[Piece]] = Array.ofDim[Piece](8, 8)

  initBoard()

  def initBoard(): Unit = {
    if (pieces.isEmpty) {
      initBoard(getStartGamePieces())
    } else {
      initBoard(pieces)
    }
  }

  def isCheckmate(pieceColor: PieceColor): Boolean = {
//    todo
    false
  }

  def isCheck(pieceColor: PieceColor): Boolean = {
    val kingPosition: Position = getKingPosition(pieceColor)
    var continue: Boolean = true
    var isCheck: Boolean = false
    for {
      y <- 0 to 7; if continue
      x <- 0 to 7; if continue
    } {
      val p:Piece = board(y)(x)
      if (p != null && p.pieceColor != pieceColor) {
        p.getPossibleMoves(this, Position(x, y)).foreach(pos => {
          if (pos.equals(kingPosition)) {
            continue = false
            isCheck = true
          }
        })
      }
    }
    isCheck
  }

  case class Check(whiteCheck: Boolean, whiteCheckmate: Boolean, blackCheck: Boolean, blackCheckmate: Boolean)

  case class KingsPositions(whiteKingPosition: Position, blackKingPosition: Position)

  def getKingPosition(pieceColor: PieceColor): Position = {
    var pos: Position = null
    var continue: Boolean = true
    for {
      y <- 0 to 7; if continue
      x <- 0 to 7; if continue
    } {
      val p:Piece = board(y)(x)
      if (p != null && p.pieceColor.eq(pieceColor) && p.pieceType.equals(King)) {
        pos = Position(x, y)
        continue = false
      }
    }
    pos
  }

  def getPiecesByColor(pieceColor: PieceColor): Set[Piece] = {
    var pieces: Set[Piece] = Set.empty
    for {
      y <- 0 to 7
      x <- 0 to 7
    } {
      val p:Piece = board(y)(x)
      if (p != null && p.pieceColor.eq(pieceColor)) {
        pieces += p
      }
    }
    pieces
  }

  def getKingsPositions(): KingsPositions = {
    var blackKingPosition: Position = null
    var whiteKingPosition: Position = null
    for {
      y <- 0 to 7
      x <- 0 to 7
    } {
      val p:Piece = board(y)(x)
      if (p.pieceType.equals(King)) {
        p.pieceColor match {
          case Black => blackKingPosition = Position(y, x)
          case White => whiteKingPosition = Position(y, x)
        }
      }
    }
    KingsPositions(whiteKingPosition, blackKingPosition)
  }

  def getPieceByPosition(position: Position): Piece = {
    board(position.y)(position.x)
  }

  def isEmptyField(position: Position): Boolean = {
    getPieceByPosition(position) == null
  }

  def isEnemyPiece(position: Position, pieceColor: PieceColor): Boolean = {
    val piece: Piece = getPieceByPosition(position)
    piece != null && piece.pieceColor != pieceColor
  }

  def isMyPiece(position: Position, pieceColor: PieceColor): Boolean = {
    val piece: Piece = getPieceByPosition(position)
    piece != null && piece.pieceColor == pieceColor
  }

  def initBoard(bP: Set[BoardPieces]): Unit = {
    bP.foreach(p => {
      board(p.position.y)(p.position.x) = p.piece
    })
  }

  def printBoard(): Unit = {
    for {
      y <- 7 to 0 by -1
      x <- 0 to 7 by +1
    } {
      print(board(y)(x) + ", ")
      if (x == 7) println()
    }
  }

  def getStartGamePieces(): Set[BoardPieces] = {
    var s : Set[BoardPieces] = Set()

    s += BoardPieces(Piece(White, Rook), Position(0, 0))
    s += BoardPieces(Piece(White, Knight), Position(1, 0))
    s += BoardPieces(Piece(White, Bishop), Position(2, 0))
    s += BoardPieces(Piece(White, Queen), Position(3, 0))
    s += BoardPieces(Piece(White, King), Position(4, 0))
    s += BoardPieces(Piece(White, Bishop), Position(5, 0))
    s += BoardPieces(Piece(White, Knight), Position(6, 0))
    s += BoardPieces(Piece(White, Rook), Position(7, 0))

    s += BoardPieces(Piece(White, Pawn), Position(0, 1))
    s += BoardPieces(Piece(White, Pawn), Position(1, 1))
    s += BoardPieces(Piece(White, Pawn), Position(2, 1))
    s += BoardPieces(Piece(White, Pawn), Position(3, 1))
    s += BoardPieces(Piece(White, Pawn), Position(4, 1))
    s += BoardPieces(Piece(White, Pawn), Position(5, 1))
    s += BoardPieces(Piece(White, Pawn), Position(6, 1))
    s += BoardPieces(Piece(White, Pawn), Position(7, 1))


    s += BoardPieces(Piece(Black, Rook), Position(0, 7))
    s += BoardPieces(Piece(Black, Knight), Position(1, 7))
    s += BoardPieces(Piece(Black, Bishop), Position(2, 7))
    s += BoardPieces(Piece(Black, King), Position(3, 7))
    s += BoardPieces(Piece(Black, Queen), Position(4, 7))
    s += BoardPieces(Piece(Black, Bishop), Position(5, 7))
    s += BoardPieces(Piece(Black, Knight), Position(6, 7))
    s += BoardPieces(Piece(Black, Rook), Position(7, 7))

    s += BoardPieces(Piece(Black, Pawn), Position(0, 6))
    s += BoardPieces(Piece(Black, Pawn), Position(1, 6))
    s += BoardPieces(Piece(Black, Pawn), Position(2, 6))
    s += BoardPieces(Piece(Black, Pawn), Position(3, 6))
    s += BoardPieces(Piece(Black, Pawn), Position(4, 6))
    s += BoardPieces(Piece(Black, Pawn), Position(5, 6))
    s += BoardPieces(Piece(Black, Pawn), Position(6, 6))
    s += BoardPieces(Piece(Black, Pawn), Position(7, 6))

    s
  }

}

case class BoardPieces(piece: Piece, position: Position)

