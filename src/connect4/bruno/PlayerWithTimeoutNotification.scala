package connect4.bruno
import connect4.Color._
import connect4._ 

/**
 * Wrapper around a player, with a timeout notification 
 * 
 */
class PlayerWithTimeoutNotification(var timeOut:Long,var preTimeoutNotification:Long,var player:NotificablePlayer) extends Player{
 
  override def getColor():Color={
   player.getColor();
  }
 
  def choice(board:Board):Integer={
 
    var startTime=System.currentTimeMillis()
	
     
    val holdResult=Array[Integer](1)
    holdResult(0)=null
    var thread=new Thread(){
	   override def run():Unit={
	    holdResult(0)=player.choice(board);
	   }
    };
   
    thread.start()
    thread.join(timeOut-preTimeoutNotification)
    var delay=System.currentTimeMillis()-startTime
    var usage=delay/(1.0+timeOut-preTimeoutNotification)
    if (holdResult(0)!=null) {
      player.postCall(holdResult(0),usage)
      
     
    } else{
      var choice=player.postCall(holdResult(0),usage)
      thread.stop()
      choice
    }
       
  }
  override def toString():String={
        player.toString();
    }



}

