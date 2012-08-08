package connect4
import Color._
object Parser {

  def readBoard(text:String,board0:Board):Board={
    
    
    var lines=text.split("\n")
    lines=lines.drop(1)
    lines=lines.reverse
    lines=lines.drop(1)
    var board=board0
    for(line <-lines){
     // Log.debug(line)
      var cols=line.split("\\|")
       
      for(i <- 1 until 8 ){
          var token=cols(i).trim();
         // Log.debug("-"+token+"-")
          if (!token.isEmpty()){
            var color=if (token.equals(Color.toString(Cross))) Cross else Circle
        //    Log.debug(color.toString())
            board=board.move(color,i-1)
          }
      }
    }
    board
  }
  
  
}