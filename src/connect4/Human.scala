package connect4
import Color._
   
/**
 * A player that reads the choice from the command line
 */
class Human(override val name:String,override val color:Color) extends BasePlayer{
    
    override def choice(board:Board):Integer={
       System.out.print(""+board);
       System.out.print("\n"+color+", your choice ? ");
       Console.readInt
     }
   
  }