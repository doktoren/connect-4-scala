package connect4

//import connect4.Board
//import connect4.Color
//import connect4.Player
import scala.util.Random

/**
 * Player cross starts
 */
class BaseBoard(val width:Int, val height:Int) {

  type Position = Int
  type BoardPattern = Long

  val d_up = 1
  val d_down = -d_up
  val d_right = height + 1
  val d_left = -d_right
  val d_up_right = d_up + d_right
  val d_up_left = d_up + d_left
  val d_down_right = d_down + d_right
  val d_down_left = d_down + d_left

  val size = width * height
  val size_plus = size + width

  val positions : Array[(Int, Int)] = {
    val seq =
      for { x <- 0 until width
            y <- 0 until height + 1
          }
        yield (if (y == height) (-1, -1) else (x, y))
    seq.toArray
  }

  def pos(column:Int, row:Int):Position = column * d_right + row

  def bit(column:Int, row:Int):BoardPattern = 1L << pos(column, row)

  /**
   * Returns a list of possible connect 4 patterns for the given position
   */
  def patterns(column:Int, row:Int):List[BoardPattern] =
    for { i <- List.range(0, 4)
      d <- List((1,0), (0,1), (1,1), (1,-1))
      if (0 <= column + (i-0)*d._1 && column + (i-0)*d._1 < width &&
          0 <= row + (i-0)*d._2 && row + (i-0)*d._2 < height)
      if (0 <= column + (i-1)*d._1 && column + (i-1)*d._1 < width &&
          0 <= row + (i-1)*d._2 && row + (i-1)*d._2 < height)
      if (0 <= column + (i-2)*d._1 && column + (i-2)*d._1 < width &&
          0 <= row + (i-2)*d._2 && row + (i-2)*d._2 < height)
      if (0 <= column + (i-3)*d._1 && column + (i-3)*d._1 < width &&
          0 <= row + (i-3)*d._2 && row + (i-3)*d._2 < height)
    }
    yield bit(column + (i-0)*d._1, row + (i-0)*d._2) |
          bit(column + (i-1)*d._1, row + (i-1)*d._2) |
          bit(column + (i-2)*d._1, row + (i-2)*d._2) |
          bit(column + (i-3)*d._1, row + (i-3)*d._2)

	val patterns_per_square =
	  positions.map(pos => patterns(pos._1, pos._2))

	/** Used by list_moves */
  val column_topbit_pairs =
    List.range(0, width).map(column => (column, 1L << (column * d_right + (height - 1))))

  def list_moves(pattern:BoardPattern) =
    column_topbit_pairs.filter(v => (pattern & v._2) == 0).map(v => v._1)

  /**
   * A list of the top row positions.
   */
  val top_row_positions =
    List.range(0, width).map(column => column * (height + 1))


  def toString(f:BoardPattern => Char):String = {
    val top_a = for ( column <- List.range(0, width) ) yield "|%2d ".format(column)
    val top_b = "|\n" :: (for ( column <- List.range(0, width) ) yield "|---")
    val b = List.flatten(
      for ( row <- List.range(height - 1, -1, -1) )
        yield "|\n" :: (for ( column <- List.range(0, width) ) yield "| %c ".format(f(bit(column, row))))
    )
    List.flatten(List(top_a, top_b, b, List("|\n")))./:\("")(_+_)
  }

}


class BitBoard(val base:BaseBoard, var board:Long, var filter:Long, var moves:List[Int]) {

  // All even values are exact scores.
  var value = 1

  val randomness = {
    for (i <- Array.range(0, base.size_plus))
      yield 2 * Random.nextInt(10)
  }

  var moves_left = base.size - moves.size
  def moves_played = base.size - moves_left

  def this(base:BaseBoard) =
    this( base,
          -1 ^ (for (column <- 0 until base.width) yield 1L << (column * base.d_right))./:\(0L)(_|_),
          0,
          List())

  def representation = board
  def player_board = board & filter
  def opponent_board = ~board & filter

  def cross_to_move() = ((moves_left ^ base.size) & 1) == 0
  def cross_board() = if (cross_to_move()) player_board else opponent_board
  def circle_board() = if (cross_to_move()) opponent_board else player_board

  def list_moves() = base.list_moves(filter)

  def insert_position(column:Int) = {
    def inner(position:Int):Int =
      (filter & (1L << position)) match {
        case 0 => position
        case _ => inner(position + base.d_up)
      }
    val result = inner(base.pos(column, 0))
    result
  }

  private def position_value(pos:Int) = {
    10 * base.patterns_per_square(pos).size + randomness(pos)
  }

  def make_move(column:Int) = {
    val pos = insert_position(column)
    board = board ^ (1L << (pos + base.d_up)) ^ filter
    filter = filter ^ (1L << pos)
    moves = pos :: moves
    value = -(value + position_value(pos))
    moves_left -= 1
  }

  def undo_move() = {
    moves_left += 1
    val pos = moves.head
    value = -value - position_value(pos)
    moves = moves.tail
    filter = filter ^ (1L << pos)
    board = board ^ (1L << (pos + base.d_up)) ^ filter
  }

  def game_is_lost() = {
    moves match {
      case Nil => false
      case last_move :: _ => {
        val b = opponent_board
        base.patterns_per_square(last_move).exists(pattern => (b & pattern) == pattern)
      }
    }
  }

  def game_has_ended() = moves_left match {
    case 0 => true
    case _ => game_is_lost()
  }

  def eval_score() = game_is_lost() match {
    case true => 0x80010000 | (moves_played << 1)
    case false => moves_left match {
      case 0 => 0
      case _ => value
    }
  }

  def score_is_exact(score:Int):Boolean = (score & 1) == 0

  /**
   * Returns (move, value) move is the computed move and score the evaluation of that move
   * @param ply a positive integer
   */
  def min_max(_ply:Int):(Int, Int) = {
    val ply = if (moves_left < _ply) moves_left else _ply
    def move_value(column:Int):Int = {
      make_move(column)
      val opponent_value = eval_score() match {
        case score if score_is_exact(score) => score
        case score => if (ply == 1) score else min_max(ply - 1)._2
      }
      undo_move()
      -opponent_value
    }
    val move_values = list_moves().map(c => (c, move_value(c)))
    // Select and return the best move
    move_values./:\(-1, Int.MinValue)((acc:(Int,Int),mv:(Int,Int)) => if (acc._2 > mv._2) acc else mv)
  }

  def compute_move() = {
    System.out.println("Compute best move for board\n"+toString())
    1 match {
      case 0 => {
        val moves = list_moves()
        Random.nextInt(moves.size)
      }
      case 1 => {
        val ply = list_moves().size match {
          case n => (Math.log(50000000.0) / Math.log(n)).intValue()
        }
        //System.out.println("Search depth is %s\n".format(ply))
        val (move, value) = min_max(ply)
        move
      }
    }
  }

  /**
   * Slow, but whatever.
   */
  override def toString():String = {
    def pos(b:Long):Char = {
      if ((b & filter) == 0)
        ' '
      else if ((b & cross_board()) != 0)
        'X'
      else
        'O'
    }
    base.toString(pos)
  }

  override def clone():BitBoard =
    return new BitBoard(base, board, filter, moves)

}

class WrapperBoard(val board:BitBoard) extends Board {

  def this(width:Int, height:Int) = this(new BitBoard(new BaseBoard(width, height)))

  def this() = this(7, 6)

  /**
   * return the list of possible moves
   */
  def possibleMoves():List[Integer] = board.list_moves().map(c => new Integer(c))

  /**
   * returns a new Board after performing a move
   */
  def move(color:Color.Color, column:Integer):Board = {
    val b = board.clone()
    b.make_move(column.intValue())
    return new WrapperBoard(b)
  }

  /**
   * return the color of the first winner found in the board (there should be only one at max.),
   * or null if no color is winning
   */
  def isWin():Color.Color = {
    if (!board.game_is_lost()) null else (if (board.cross_to_move()) Color.Circle else Color.Cross)
  }

  /**
   * returns an array of Colors , indicating the state of the board
   * first dimension is the rows, second is the columns
   */
  def getBoard() : Array[Array[Color.Color]] = {
    val base = board.base
    var result = Array.ofDim[Color.Color](base.height, base.width)
    val cross = board.cross_board()
    for (y <- 0 until base.height)
      for (x <- 0 until base.width) {
        val bit = base.bit(x,y)
        result(y)(x) =
          if ((bit & board.filter) == 0)
            null
          else if ((bit & cross) != 0)
            Color.Cross
          else
            Color.Circle
      }
    result
  }

}


class Engine(color:Color.Color) extends Player {

  /**
   * Maybe theoretically could fail, but let's assume that it doesn't...
   */
  def convert(b:Board):BitBoard = {
    val array_board = b.getBoard()
    val base = new BaseBoard(array_board(0).size, array_board.size)
    var board = new BitBoard(base)
    val heights = Array.ofDim[Int](base.width)
    var player = Color.Cross
    def map_row(row:Int) = (base.height - 1) - row
    def columnToPlay():Int =
      {
        for (column <- 0 until base.width) {
          val row = heights(column)
          if (row + 3 < base.height  &&
              array_board(map_row(row))(column) == player  &&
              array_board(map_row(row + 1))(column) == Color.invert(player)  &&
              array_board(map_row(row + 2))(column) == Color.invert(player)  &&
              array_board(map_row(row + 3))(column) == Color.invert(player)) {
            heights(column) += 1
            return column
          }
        }
        for (column <- 0 until base.width) {
          val row = heights(column)
          if (row + 2 < base.height  &&
              array_board(map_row(row))(column) == player  &&
              array_board(map_row(row + 1))(column) == Color.invert(player)  &&
              array_board(map_row(row + 2))(column) == Color.invert(player)) {
            heights(column) += 1
            return column
          }
        }
        for (column <- 0 until base.width) {
          val row = heights(column)
          if (row + 1 < base.height  &&
              array_board(map_row(row))(column) == player  &&
              array_board(map_row(row + 1))(column) == Color.invert(player)) {
            heights(column) += 1
            return column
          }
        }
        for (column <- 0 until base.width) {
          val row = heights(column)
          if (row < base.height  &&
              array_board(map_row(row))(column) == player) {
            heights(column) += 1
            return column
          }
        }
        for (column <- 0 until base.width) {
          val row = heights(column)
          if (row < base.height  &&  array_board(map_row(row))(column) != null)
            error("Failed to convert board")
        }
        return -1
      }
    var column = columnToPlay()
    while (column != -1) {
      board.make_move(column)
      player = Color.invert(player)
      column = columnToPlay()
    }
    /*
    for (column <- 0 until base.width)
      for (row <- 0 until base.height) {
        val piece = array_board(map_row(row))(column)
        if (piece != null) {
          val b = base.bit(column, row)
          board.filter |= b
          board.board |= b << base.d_up
          if (piece == Color.Circle)
            board.board ^= b
          board.moves = base.pos(column, row) :: board.moves
          board.moves_left -= 1
        }
      }
    if ((board.moves_played & 1) != 0)
      board.board ^= board.filter
    */
    board
  }

  def choice(board:Board):Integer = {
    val b = convert(board)
    if (b.game_has_ended()) -1 else new Integer(b.compute_move())
  }

  def getColor():Color.Color = color

  override def toString():String = "Jespers engine"
}
