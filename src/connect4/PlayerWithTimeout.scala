package connect4
import Color._
   

/**
 * Wrapper around a player, with a timeout. 
 * If the player has not taken any decision
 * when the timeout expires, 
 * a random choice is picked
 */
class PlayerWithTimeout(var timeOut:Long,var player:Player) extends Player{
     
	 override def getColor():Color={
       player.getColor();
     }
     
     def choice(board:Board):Integer={
     
       val holdResult=Array[Integer](1)
       holdResult(0)=null
       var thread=new Thread(){
    	   override def run():Unit={
    	    holdResult(0)=player.choice(board);
    	   }
       };
       thread.start()
       thread.join(timeOut)
       
       if (holdResult(0)!=null)
         holdResult(0)
       else{
         System.err.println("timeout for player "+player+" choosing random move");
         getRandomMove(board);
       }
       
     }
     override def toString():String={
        player.toString();
    }
  }
   