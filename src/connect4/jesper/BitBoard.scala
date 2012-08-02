
import connect4.Board
import connect4.Color

// TODO: Invert y-axis for compability with Board.scala

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

	val compute_patterns_per_square =
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

  def this(base:BaseBoard) =
    this( base,
          (for (column <- 0 until base.width) yield 1L << (column * base.d_right))./:\(0L)(_|_),
          0,
          List())

  def cross_to_move() = (moves.length & 1) == 0

  /*
  def get(column:Int, row:Int) = {
    val b = base.bit(column, row)
    if ((b & filter) == 0)
      null
    else if (((b & board) != 0) == cross_to_move())
      Color.Cross
    else
      Color.Circle
  }
  */

  def representation = board
  def player_board = board & filter
  def opponent_board = ~board & filter
  def cross_board() = if (cross_to_move()) player_board else opponent_board
  def circle_board() = if (cross_to_move()) player_board else opponent_board

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

  def make_move(column:Integer) = {
    val pos = insert_position(column)
    board = board ^ (1L << (pos + base.d_up)) ^ filter
    filter = filter ^ (1L << pos)
    moves = pos :: moves
  }

  def undo_move() = {
    val pos = moves.head
    moves = moves.tail
    filter = filter ^ (1L << pos)
    board = board ^ (1L << (pos + base.d_up)) ^ filter
  }

  override def toString():String = {
    def pos(b:Long):Char = {
      if ((b & filter) == 0)
        ' '
      else if (((b & board) == 0) == cross_to_move())
        'X'
      else
        'O'
    }
    base.toString(pos)
  }

  override def clone():BitBoard =
    return new BitBoard(base, board, filter, moves)

}

class WrapperBoard extends Board {

  def convert(b:Board):BitBoard = {
    val array_board = b.getBoard()
    val base = new BaseBoard(array_board.size, array_board(0).size)
    var board = new BitBoard(base)
    val heights = Array.ofDim[Int](base.width)
    var player = Color.Cross
    def map_row(row:Int) = (base.height - 1) - row
    def columnToPlay():Int =
      {
        for (column <- 0 until base.width) {
          if (heights(column) < base.height  &&  array_board(column)(map_row(heights(column))) == player) {
            heights(column) += 1
            return column
          }
        }
        return -1
      }
    var column = columnToPlay()
    while (column != -1) {
      board.make_move(column)
      player = Color.invert(player)
      column = columnToPlay()
    }
    board
  }

  var board = new BitBoard(new BaseBoard(7, 6))

  /**
   * return the list of possible moves
   */
  def possibleMoves():List[Integer] = board.list_moves().map(c => new Integer(c))

  /**
   * returns a new Board after performing a move
   */
  def move(color:Color.Color, column:Integer):Board = {
    val c = column.intValue()
    // TODO: clone
    return this
  }

  /**
   * return the color of the first winner found in the board (there should be only one at max.),
   * or null if no color is winning
   */
  def isWin():Color.Color = {
    // TODO
    return null
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

object Test {
  def main(args : Array[String]) : Unit = {
    val base = new BaseBoard(7, 6)
    var board = new BitBoard(base)
    val actions = "33333344uuu06"
    actions.foreach( action => {
      action match {
        case 'u' => board.undo_move()
        case _ => board.make_move(action - '0')
      }
      System.out.println(board.toString())
      System.out.println("Valid moves: " + board.list_moves().map(String.valueOf).reduce(_ + ", " + _))
    })
  }
}
