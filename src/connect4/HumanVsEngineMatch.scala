package connect4

object HumanVsEngineMatch {
  def main(args : Array[String]) : Unit = {
    System.err.println("winner:"+new Match(
        new PlayerWithTimeout(30000,
            new Human("A",Color.Cross))
        ,new PlayerWithTimeout(10000,
            new Engine(Color.Circle))
        ,new BoardImpl).playTillEnd())
    
  }
}