package connect4
//import connect4.bruno.Explorer

/**
 * a Match between two humans
 */
object HumanVsHumanMatch {
 def main(args : Array[String]) : Unit = {
    
    
    
   System.err.println("winner:"+new Match(
       new PlayerWithTimeout(30000,
           new Human("A",Color.Cross))
       ,new PlayerWithTimeout(30000,
           new Human("B",Color.Circle))
       ,new BoardImpl).playTillEnd())
    
  }
}