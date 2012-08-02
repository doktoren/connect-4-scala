package connect4
import Color._

/**
 * A base implementation of the board. 
 * Feel free to make a faster one if you wish :)
 */
class BoardImpl extends Board{
    
    def width = 7
    def height = 6
  
    def copy():BoardImpl={
      var retVal=new BoardImpl();
      retVal.board = board.map(_.clone)
      return retVal
      
    }
    var board:Array[Array[Color]]=emptyBoard()
    override def getBoard():Array[Array[Color]]={
    	board	
    }
    def emptyBoard():Array[Array[Color]]={
      var board=Array.ofDim[Color](height, width)
      for (y <- 0 until height) {
         for (x <- 0 until width)
          board(y)(x)=null
      }
      board
    }
    
    
    /*
     * look 4 same color in vertical sequence
     */
    def look4vert():Color={
      for (x <- 0 until width)
         for (y <- 0 until height - 3) {
            var color=board(y)(x);
            if(color!=null &&
               board(y+1)(x)==color &&
               board(y+2)(x)==color &&
               board(y+3)(x)==color)
               return color;
           }
      null
    }
    /*
     * look 4 same color in vertical sequence
     */
     def look4horiz():Color={
       for (y <- 0 until height)
         for (x <- 0 until width - 3){
            var color=board(y)(x);
            if(color!=null &&
               board(y)(x+1)==color &&
               board(y)(x+2)==color &&
               board(y)(x+3)==color)
               return color;
           }
      null
    }
    
     
     /*
     * look for a sequence of 4 same color in diagonal toward south-east 
     */
     def look4diag2se():Color={
       for (y <- 0 until height - 3)
         for (x <- 0 until width - 3){
            var color=board(y)(x);
            if(color!=null &&
               board(y+1)(x+1)==color &&
               board(y+2)(x+2)==color &&
               board(y+3)(x+3)==color)
               return color;
           }
      null
    }
    def isWin():Color={
     var color:Color=null
     color=look4vert()
     if (color!=null) return color
     color=look4horiz()
     if (color!=null) return color
     color=look4diag2se()
     if (color!=null) return color
     color=look4diag2ne()
     if (color!=null) return color
     null
    }
     
      /*
     * look for a sequence of 4 same color in diagonal toward north-east 
     */
     def look4diag2ne():Color={
       for (y <- 0 until height - 3)
         for (x <- 0 until width - 3){
            var color=board(y)(x+3);
            if(color!=null &&
               board(y+1)(x+2)==color &&
               board(y+2)(x+1)==color &&
               board(y+3)(x)==color)
               return color;
           }
      null
    }
     
   override def possibleMoves():List[Integer]={
     var retVal=List[Integer]()
     for(x<-0 until width){
        if (board(0)(x)==null)
          retVal::=x
     }
     retVal
   }  
    
   
    
    
    /**
     * return the y at which a token would fall if 
     * thrown in the column x
     */
    def fall2(x:Integer):Integer={
    //   Log.debug("fall2("+x+")");
      for (y <- 0 until height) {
      //   Log.debug("board("+y+")("+x+")="+board(y)(x))
         if (board(y)(x)!=null){
            return y-1
          }
        }
        return height - 1
    }
    
    override def move(color:Color,position:Integer):Board={
      var y=fall2(position)      
      var retVal=copy();
      retVal.board(y)(position)=color
    //  Log.debug("move("+color+","+position+") set token to y="+y);
    //  Log.debug(""+retVal.board(y)(position));
    //  Log.debug(retVal.toString());
      
      return retVal
    }
    
    override def toString():String={
      var retVal=""
      for (y <- 0 until height) {
         retVal+="\n"+y+" | ";
         for (x <- 0 until width) {
           if (board(y)(x)==null) 
             retVal+=" "//+x+","+y
           else
             retVal+=Color.toString(board(y)(x))
           retVal+=" | " 
         }
      }
      retVal+="\n    "
      for (x <- 0 until width) 
        retVal+=x+"   "
        
      
      retVal 
    }
    def getPredicateInfo():String={
      var retVal=""
      retVal+="\n  isWin:"+isWin()  
      retVal+="\n  vert:"+look4vert()
      retVal+="\n  horiz:"+look4horiz() 
      retVal+="\n  diag2se:"+look4diag2se()
      retVal+="\n  diag2ne:"+look4diag2ne()
      
      retVal+="\n  possibleMoves:"+possibleMoves()
      retVal
      
    }
    
    
     def test(args : Array[String]) : Unit = {  
    var board0=new BoardImpl;
    System.out.print(""+board0);
    
    var board1=board0.move(Color.Circle,3);
    var board2=board1.move(Color.Cross,3);
    var board3=board0.move(Color.Circle,4);
   
    System.out.println(board1);
     System.out.println(board2);
     System.out.println(board3);
     
     
     System.out.println(board0.move(Color.Circle,7).move(Color.Circle,7).move(Color.Circle,7).move(Color.Circle,7));
     
     System.out.println(board0.move(Color.Cross,7).move(Color.Cross,7).move(Color.Cross,7).move(Color.Circle,7).move(Color.Circle,7).move(Color.Circle,7).move(Color.Circle,7));
  
     
     
     
     System.out.println(board0.move(Color.Circle,7).move(Color.Circle,6).move(Color.Circle,5).move(Color.Circle,4));
     
     
     System.out.println(board0
			    		 .move(Color.Circle ,5).move(Color.Circle ,6).move(Color.Circle ,7)
					     	                .move(Color.Circle ,6).move(Color.Circle ,7)
					     	                                   .move(Color.Circle ,7)
					
	  .move(Color.Cross,4).move(Color.Cross,5).move(Color.Cross,6).move(Color.Cross,7));

     
    
     System.out.println(board0
		 .move(Color.Circle ,4).move(Color.Circle ,5).move(Color.Circle ,6)
	     .move(Color.Circle ,4).move(Color.Circle ,5)
	     .move(Color.Circle ,4)
	
	     .move(Color.Cross,4).move(Color.Cross,5).move(Color.Cross,6).move(Color.Cross,7));

     
     
     
       System.out.println(board0
			    		 
           .move(Color.Circle ,5)
           .move(Color.Circle ,5)
           .move(Color.Circle ,5)
           .move(Color.Circle ,5)
           .move(Color.Circle ,5)
           .move(Color.Circle ,5)
           .move(Color.Circle ,5))
     
  }
  
    
  }