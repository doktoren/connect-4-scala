package connect4.bruno
import connect4._

object RandomVsComputerTest {
 def main(args : Array[String]) : Unit = {
    
    
    
   System.err.println("winner:"+new Match(
       new PlayerWithTimeout(5000,
           //new Human("human",Color.Red)),
            new RandomPlayer("rnd",Color.Cross))
       ,new PlayerWithTimeout(5000,
           new Explorer("pc",Color.Circle,6))
       ,new BoardImpl).playTillEnd())
    //  play(new Human(Color.Red),new Human(Color.Blue),new Board)
   
     
    
    
  }
}