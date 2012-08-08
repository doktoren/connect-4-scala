package connect4.bruno
import connect4.Color._
import connect4._
import scala.util.Random




class Explorer(override val name:String,override val color:Color,val initialMovesDepth:Integer) extends BasePlayer with NotificablePlayer{
  
  
  var evaluatedCnt=0;
  var movesDepth=initialMovesDepth;
  
  override def postCall(choice:Integer,timeUsed:Double):Integer={
    Log.info("postCall("+choice+","+timeUsed+")")
    if (choice==null){
      movesDepth-=1
      Log.info("************************* movesDepth decreased to: "+movesDepth)
    }
    else 
      if (timeUsed<0.30){    
        movesDepth+=1
        Log.info("movesDepth increased to: "+movesDepth)
      }  
    choice  
  }
  
  
  override def choice(board:Board):Integer={
     evaluatedCnt=0
      var fastBoard=new FastBoardImpl().init(board)
     var retVal=getBestMoveAndScore(fastBoard,color,movesDepth)._2;     
  //   Log.info("evaluatedCnt="+(evaluatedCnt+1));
     retVal
  }
  
   override def toString():String={
     "pc "+color+" (depth:"+movesDepth+")";     
  }
   
  
  def evaluateScore(board:Board,color:Color,depth:Integer):Double={
    
 //   Log.debug("evaluateScore() evaluatedCnt:"+evaluatedCnt);
     var winner=board.isWin()
     if (winner!=null){
       if (winner==color)  {
 //        Log.info(board.toString())
         99
       }
       else
         -99; // never happens, coz we evaluate only after moving our color
     } else {
 //      Log.debug("depth:"+depth);
       if (depth>0){
         evaluatedCnt+=1
    	 -getBestMoveAndScore(board,Color.invert(color),depth-1)._1
       }
       else{
         0
       }
     }
  }
  
  var randomGen=new Random(1)

   def getBestMoveAndScore(board:Board,color:Color,depth:Integer):(Double,Integer)={
   //  Log.debug("getBestMoveAndScore()");
     var scoreNotYetSet=true
     var bestScore:Double= 0;
  //   var bestPosition:Integer= -1
     
     var allBest:List[Integer]=List[Integer]()
   //  Log.debug("exploring all child:"+board.possibleMoves());
     for(x<-board.possibleMoves()){
    //     Log.debug(color+" trying move "+x);
         var afterMove=board.move(color,x);
         var score=evaluateScore(afterMove,color,depth)
         if (scoreNotYetSet || bestScore<=score){
           if (scoreNotYetSet || bestScore<score){ // new best score
             scoreNotYetSet=false
             bestScore=score
      //       bestPosition=x
             allBest=List[Integer](x)
          //   Log.debug("found new best score "+bestScore+" @"+bestPosition);
           } else { // another best score with same value
             allBest::=x
          //   Log.debug("found additional best score "+bestScore+" @"+bestPosition+" allBest:"+allBest);
           }
         }
     }
     
     // no choice possible?
     if (allBest.size==0){
       return (0,-1)
     }
     
     
     
     
     // pick any of the best
     //var choosenIdx=(randomGen.nextDouble()*allBest.size).intValue()
     //var choosenBest=allBest(choosenIdx)
     var choosenBest=getMostCentral(allBest)
     
   //  Log.debug("returning best score "+choosenBest+" @"+choosenBest);
     return (bestScore,choosenBest)
   }
  
  def getMostCentral(choices:List[Integer] ):Integer={
    var best:Integer=null
    var distanceBest:Integer=null
    for(choice<-choices)
      if (best==null){
        best=choice
        distanceBest=Math.abs(choice-3)
      }
        
      else {
        var distance=Math.abs(choice-3)
          
        if (distance<distanceBest){
          best=choice
          distanceBest=Math.abs(choice-3)
        }
            
      }
    
    best
  }
  
  
  
  
}