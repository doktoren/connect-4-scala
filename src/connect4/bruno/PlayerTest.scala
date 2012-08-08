package connect4.bruno
import connect4.Color._
import connect4._


object PlayerTest {

  def main(args : Array[String]) : Unit = {
    
   assert(3==new Explorer("",Color.Cross,3).getMostCentral(List[Integer](0,1,2,3,4,5,6)))
   
      
   assert(5==new Explorer("",Color.Cross,5).getMostCentral(List[Integer](5,0)))
   
   var test1="\n"+
"0 |   |   |   |   |   | o | o | \n"+ 
"1 |   |   |   |   | x | x | x | \n"+
"2 |   | o |   | x | o | x | o | \n"+
"3 | o | x |   | o | x | o | x | \n"+ 
"4 | o | x |   | o | o | x | x | \n"+ 
"5 | x | x | o | x | x | o | o | \n"+
"    0   1   2   3   4   5   6   "   


    
   var test2="\n"+
"0 |   |   |   |   |   |   | x | \n"+ 
"1 |   |   |   | o | x |   | x | \n"+ 
"2 |   |   |   | o | o |   | o | \n"+ 
"3 |   | o |   | x | x |   | x | \n"+ 
"4 | o | x | o | o | o |   | x | \n"+ 
"5 | o | x | o | o | x | x | x | \n"+ 
"    0   1   2   3   4   5   6   ";

   
   testChoice(new Explorer("",Color.Cross,3),test2)
    
    
  }
     def testChoice(player:Player,boardAsString:String) : Unit = {
  


   var board0=Parser.readBoard(boardAsString,new BoardImpl())
   
   
   
   Log.info("choice:"+player.choice(board0))
    
	
   }
}
