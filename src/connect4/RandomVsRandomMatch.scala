package connect4
//import connect4.bruno.Explorer

/**
 * a Match between two random players
 */
object HumanVsRandomMatch {
 def main(args : Array[String]) : Unit = {
    
    
    
   System.err.println("winner:"+new Match(
       new PlayerWithTimeout(30000,
           new RandomPlayer("A",Color.Cross))
       ,new PlayerWithTimeout(30000,
           new RandomPlayer("B",Color.Circle))
       ,new BoardImpl).playTillEnd())
    
  }
}