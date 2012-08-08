package connect4.bruno
import connect4.Color._
import connect4._ 

trait NotificablePlayer extends Player{
  def postCall(choice:Integer,timeUsed:Double):Integer;
}