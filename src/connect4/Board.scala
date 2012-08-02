package connect4
import Color._

/**
 * interface for a board
 */
trait Board {
  
  /**
   * return the list of possible moves
   */
   def possibleMoves():List[Integer]
   
   /**
    * returns a new Board after performing a move
    */
   def move(color:Color, column:Integer):Board
   
   /**
    * return the color of the first winner found in the board (there should be only one at max.),
    * or null if no color is winning
    */
   def isWin():Color
   
   /**
    * returns an array of Colors , indicating the state of the board
    * first dimension is the rows, second is the columns
    */
   def getBoard():Array[Array[Color]]

}
