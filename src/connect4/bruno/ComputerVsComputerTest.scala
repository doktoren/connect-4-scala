package connect4.bruno
import connect4._

object ComputerVsComputerTest {
 def main(args : Array[String]) : Unit = {
    
    
    var explorer=new ExplorerWithCache("pc",Color.Cross,8)
   System.err.println("winner:"+new Match(   
       new PlayerWithTimeout(10000,
           new PlayerWithTimeoutNotification(10000-200,200,explorer)),
       new PlayerWithTimeout(10000,
           new Engine(Color.Circle)),
       new BoardImpl).playTillEnd())
   
//     
//   System.err.println("winner:"+new Match(
//       new PlayerWithTimeout(5000,
//            new Explorer("pc",Color.Cross,8))
//       ,new PlayerWithTimeout(5000,
//           new Explorer("pc",Color.Circle,8))
//       ,new BoardImpl).playTillEnd())
//    
//    
  }
}