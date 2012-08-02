package connect4
import Color._

/**
 * 'interface' for a player
 */
trait Player{
     def getColor():Color
     
     /**
      * returns the choice of the player for next move
      * -1 indicates that the player does not want or can't play this turn
      */
     def choice(board:Board):Integer
     
     /**
      * convenience method
      * returns a random move from the possible moves
      */
     def getRandomMove(board:Board):Integer={ 
        if (board.possibleMoves().isEmpty)
          -1
        else {
    	  var indexChosen=(Math.random*board.possibleMoves().size).intValue();
          board.possibleMoves()(indexChosen);
        }
   }
 }
   