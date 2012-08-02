package connect4
import Color._
   
/**
 * Convenience class for the Player.
 *  
 * I could not find a way to define the color in Player and
 * then override it in 'constructor' in PlayerWithTimeout,
 * so I put it in a sepearate class
 * 
 */
trait BasePlayer extends Player {
     val color:Color=null;
     val name:String=null;
   
     override def getColor():Color={
       color
     }
     override def toString():String={
        name+", "+color+" ";     
    }
  }
   