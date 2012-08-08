package connect4

object ParserTest {

   def main(args : Array[String]) : Unit = {
     var test="\n"+
"0 |   |   |   |   |   | o | o | \n"+ 
"1 |   |   |   |   | x | x | x | \n"+
"2 |   | o |   | x | o | x | o | \n"+
"3 | o | x |   | o | x | o | x | \n"+ 
"4 | o | x |   | o | o | x | x | \n"+ 
"5 | x | x | o | x | x | o | o | \n"+
"    0   1   2   3   4   5   6   "   
  
  
  Log.debug(test)
  var board1=Parser.readBoard(test,new BoardImpl())
  Log.debug(board1.toString())
  assert(test.equals(board1.toString()))
  }
  
   
}