package connect4.bruno

import connect4.Color._
import connect4._


class FastBoardImpl() extends Board{ 

    val COLUMNS=7
    val ROWS=6
    
    var oneDimBoard=Array.ofDim[Int](COLUMNS*ROWS);
    var cachedHashCode:Int=0
//    var cachedPossibleMoves:List[Int]
     
    def init(initialBoard:Board):FastBoardImpl={
      var idx=0
      for (y <- 0 until 6) {
        for (x <- 0 until 7) {
    	  var color=initialBoard.getBoard()(y)(x)
          oneDimBoard(idx)=color2int(color)
          cachedHashCode+=colorHash(idx,color2int(color))
          idx+= 1;
    	}
      }
      
//      cachedPossibleMoves=List[Int]() 
//      for (x <- 0 until 8) 
//        if (oneDimBoard(x)==null)
//      	  cachedPossibleMoves::=x
      	  
      this 
    }
    val INT_CIRCLE=1
    val INT_CROSS=2
    val INT_EMPTY=0
    
    
    def color2int(color:Color):Int=color match {
	            case Circle=>INT_CIRCLE
	            case Cross =>INT_CROSS
	            case _=> INT_EMPTY
            }
    
     def int2color(color:Int):Color=color match {
	            case INT_CIRCLE=>Circle
	            case INT_CROSS =>Cross
	            case INT_EMPTY=> null
            }
    
    
    def colorHash(idx:Int,color:Int):Int = color match {
	            case INT_CIRCLE=>idx*883
	            case INT_CROSS =>idx*997
	            case _=> 0
            }
      
    def getBoard():Array[Array[Color]]={
      var board=Array.ofDim[Color](ROWS,COLUMNS)
      
      var idx=0
      
        for (y <- 0 until ROWS) 
          for (x <- 0 until COLUMNS){
            //Log.debug("y:"+y+"x:"+x+" idx:"+idx+" v:"+oneDimBoard(idx))
            board(y)(x)=int2color(oneDimBoard(idx))
            idx+=1
        }
      board
    }
    
     var columnLastMove:Int= -1;
     var rowLastMove:Int= -1;
     var idxLastMove:Int= -1;
    /**
    * returns a new Board after performing a move
    */
    override def move(color:Color,column:Integer):Board={
      
      //Log.debug("move(color:"+color+",x:"+column+") y:"+row+" idx:"+idx+"");
      
      var retVal=new FastBoardImpl()
      
      retVal.columnLastMove=column
      retVal.rowLastMove=fall2(column)
      retVal.idxLastMove= retVal.rowLastMove*COLUMNS+column
      
      Array.copy(oneDimBoard,0,retVal.oneDimBoard,0,COLUMNS*ROWS)
      retVal.oneDimBoard(retVal.idxLastMove)=color2int(color)
      retVal.cachedHashCode+=this.cachedHashCode+colorHash(retVal.idxLastMove,color2int(color))
      retVal
    }
    
    
    
    
    
    
    
    
    
    
     /**
     * return the y at which a token would fall if 
     * thrown in the column x
     */
    def fall2(x:Int):Int={
      var idx=0
       for (y <- 0 until ROWS) {
         if (oneDimBoard(idx+x)!=0){
            return y-1
          }
         idx+=COLUMNS
        }
        return ROWS-1
    }
    
    
    override def equals(other: Any) = other match { 
      case that: FastBoardImpl =>that.oneDimBoard.sameElements(this.oneDimBoard)
      case that: BoardImpl =>new FastBoardImpl().init(that).equals(this)
      case _ => false 
    }

    override def hashCode() = cachedHashCode
    
    
    
//    def possibleMoves():List[Int]=cachedPossibleMoves
//  
//    
//    
//    
    
    
     override def toString():String={
      var retVal=""
      var idx=0
      for (y <- 0 until ROWS) {
         retVal+= "\n"+y+" | ";
         for (x <- 0 until COLUMNS) {
           if (oneDimBoard(idx)== 0) 
             retVal+= " " //+x+","+y
           else
             retVal+= Color.toString(int2color(oneDimBoard(idx)))
          retVal+= " | " 
          idx+= 1
         }
      }
      retVal+= "\n    "
      for (x <- 0 until COLUMNS){ 
        retVal+= x
        retVal+= "   "
      }
        
      
      retVal toString()//+getPredicateInfo()
    }
    
    // -- not optimized--
    
    override def possibleMoves():List[Integer]={
     var retVal=List[Integer]()
     for(x<-0 until COLUMNS){
        if (oneDimBoard(x)== 0)
          retVal::=x
     }
     retVal
   }  
    
    
    
    
     /*
     * look 4 same color in vertical sequence
     */
    def look4vertCheckAll():Int={
      var idx=0
         for (y <- 0 until ROWS-3) 
         for (x <- 0 until COLUMNS){
            //var idx=y*COLUMNS+x
         //   Log.debug("y:"+y+"x:"+x+" idx:"+idx+" v:"+oneDimBoard(idx))        
            var color=oneDimBoard(idx);
            if(color!= 0)
              if(oneDimBoard(idx+COLUMNS  )==color &&
               oneDimBoard(idx+COLUMNS+COLUMNS)==color &&
               oneDimBoard(idx+COLUMNS+COLUMNS+COLUMNS)==color)
               return color;
            idx+=1
           }
      0
    }
    /*
     * look 4 same color in vertical sequence
     */
     def look4horizCheckAll():Int={
       var idx=0
         for (y <- 0 until ROWS) {
           for (x <- 0 until COLUMNS-3) {
            //var idx=y*COLUMNS+x
            var color=oneDimBoard(idx);
            if(color!= 0 &&
               oneDimBoard(idx+1)==color &&
               oneDimBoard(idx+2)==color &&
               oneDimBoard(idx+3)==color)
               return color;
           idx+=1
           }
           idx+=3
         }
           
      0
    }
    
     
     /*
     * look for a sequence of 4 same color in diagonal toward south-east 
     */
     def look4diag2seCheckAll():Int={
       var idx=0
         for (y <- 0 until ROWS-3){
           for (x <- 0 until COLUMNS-3){
             //var idx=y*COLUMNS+x
             var color=oneDimBoard(idx);
             if(color!= 0 &&
               oneDimBoard(idx+COLUMNS  +1)==color &&
               oneDimBoard(idx+COLUMNS+COLUMNS+2)==color &&
               oneDimBoard(idx+COLUMNS+COLUMNS+COLUMNS+3)==color)
               return color;
             idx+=1
             }
           idx+=3
         }
           
      0
    }
     
     
    /*
     * look for a sequence of 4 same color in diagonal toward north-east 
     */
     def look4diag2neCheckAll():Int={
       var idx=0
         for (y <- 0 until ROWS-3){
           for (x <- 0 until COLUMNS-3){
             //var idx=y*COLUMNS+x
             var color=oneDimBoard(idx+3);
             if(color!= 0 &&
               oneDimBoard(idx+COLUMNS  +2)==color &&
               oneDimBoard(idx+COLUMNS*2+1)==color &&
               oneDimBoard(idx+COLUMNS*3  )==color)
               return color;
             idx+=1
             }
           idx+=3
           }
       	   
      0
    }
     
    def isWinCheckAll():Int={
     var color:Int=0
     color=look4vertCheckAll()
     if (color!=0) return color
     color=look4horizCheckAll()
     if (color!=0) return color
     color=look4diag2seCheckAll()
     if (color!=0) return color
     color=look4diag2neCheckAll()
     if (color!=0) return color
     0
    }
    
//      var columnLastMove:Int= -1;
//     var rowLastMove:Int= -1;
//     var idxLastMove:Int= -1;
        
     /*
     * look 4 same color in vertical sequence
     */
    def look4vertReduced():Int={
      var idx=idxLastMove
        for (y <- rowLastMove until ROWS-3) {
          var color=oneDimBoard(idx);
          if(color!=0)
            if(oneDimBoard(idx+COLUMNS  )==color &&
             oneDimBoard(idx+COLUMNS+COLUMNS)==color &&
             oneDimBoard(idx+COLUMNS+COLUMNS+COLUMNS)==color)
             return color;
          idx+=COLUMNS
        }
      0
    }
    /*
     * look 4 same color in horiz sequence
     */
     def look4horizReduced():Int={
       var idx=rowLastMove*COLUMNS
       
         for (x <- 0 until COLUMNS-3) {
            //var idx=y*COLUMNS+x
            var color=oneDimBoard(idx);
            if(color!=0 &&
               oneDimBoard(idx+1)==color &&
               oneDimBoard(idx+2)==color &&
               oneDimBoard(idx+3)==color)
               return color;
           idx+=1
           }
          
         
           
      0
    }
    
     
     /*
     * look for a sequence of 4 same color in diagonal toward south-east 
     */
     def look4diag2seReduced():Int={
       var idx=0
         for (y <- 0 until ROWS-3){
           for (x <- 0 until COLUMNS-3){
             //var idx=y*COLUMNS+x
             var color=oneDimBoard(idx);
             if(color!=0 &&
               oneDimBoard(idx+COLUMNS  +1)==color &&
               oneDimBoard(idx+COLUMNS+COLUMNS+2)==color &&
               oneDimBoard(idx+COLUMNS+COLUMNS+COLUMNS+3)==color)
               return color;
             idx+=1
             }
           idx+=3
         }
           
      0
    }
     
     
    /*
     * look for a sequence of 4 same color in diagonal toward north-east 
     */
     def look4diag2neReduced():Int={
       var idx=0
         for (y <- 0 until ROWS-3){
           for (x <- 0 until COLUMNS-3){
             //var idx=y*COLUMNS+x
             var color=oneDimBoard(idx+3);
             if(color!=0 &&
               oneDimBoard(idx+COLUMNS  +2)==color &&
               oneDimBoard(idx+COLUMNS*2+1)==color &&
               oneDimBoard(idx+COLUMNS*3  )==color)
               return color;
             idx+=1
             }
           idx+=3
           }
       	   
      0
   }
   def isWinReduced():Int={
     var color:Int=0
     color=look4vertReduced()
     if (color!=0) return color
     color=look4horizReduced()
     if (color!=0) return color
     color=look4diag2seReduced()
     if (color!=0) return color
     color=look4diag2neReduced()
     if (color!=0) return color
     0
   }
    
     def isWin():Color={
       int2color(
         if (rowLastMove== -1)
           isWinCheckAll()
         else
           isWinReduced()
         )
     }
}