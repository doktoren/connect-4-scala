package connect4
import Color._
   
/**
 * A player that makes random choices.. all the time
 */
class RandomPlayer(override val name:String,override val color:Color) extends BasePlayer{
    
    override def choice(board:Board):Integer={
       getRandomMove(board)
     }
   
  }