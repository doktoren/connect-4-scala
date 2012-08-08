package connect4.bruno
import connect4.Color._
import connect4._ 
import scala.collection.mutable.HashMap
class ExplorerWithCache(override val name: String, override val color: Color, override val initialMovesDepth: Integer) extends Explorer(name,color,initialMovesDepth){
  
   var evaluationSkipped=0;
   
   var cache:HashMap[Board,(Double,Integer)]=null
   override def choice(board:Board):Integer={
		 evaluationSkipped=0;  
	     cache=new HashMap[Board,(Double,Integer)]()
	     val retVal=super.choice(board)
	     Log.info("evaluationSkipped:"+evaluationSkipped);
	     retVal
	  }
   
   
   // TODO override hashCode and equal for board
   
   
   override def getBestMoveAndScore(board:Board,color:Color,depth:Integer):(Double,Integer)={
     cache.get(board) match {
       case Some((d,i)) => {         
       //  Log.debug("found in cache");
         evaluationSkipped+=1
         (d,i)
       }
       case None => { 
       //  Log.debug("evaluating:"+board);
         var scoreAndMove=super.getBestMoveAndScore(board,color,depth);
    	 cache.put(board,scoreAndMove)
    	 scoreAndMove
       }
     }
   }
}

