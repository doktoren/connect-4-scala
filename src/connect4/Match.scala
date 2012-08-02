package connect4



/**
 * runs a match between two players
 * it will run in a loop until  one of the two players win , or there is 
 * no more move possible
 */
class Match(playerA:Player,playerB:Player,board0:Board) {

  import Color._
  
  var board=board0;
  
  def playOneTurn(player:Player):Unit={
    System.out.println("\n\n");
    System.out.println(board);
    System.out.println("\n\n");
    System.out.println("Turn for "+player+"")
    var choice:Integer=null
	
	// must be a nice construct for that
    // repeating this line is lame
	choice=player.choice(board);
	while(choice!= -1 && !board.possibleMoves().contains(choice)){
	  System.out.print(choice+" is a wrong value!");
	  choice=player.choice(board);
	}
	
	
	if (choice== -1) 
	  System.out.println(player+" skipped the turn! ");
	else {
	  System.out.println(player+" choosed "+choice);
	  board=board.move(player.getColor(),choice);
	  
	}
  }
 
  def playTillEnd():Player={
    
    while(board.isWin()==null && !board.possibleMoves().isEmpty){
    	for(player:Player<-List(playerA,playerB)){
    	    // this test is to replace a break
    	    if (board.isWin()==null){
	    	    playOneTurn(player);
	    	}
	    }
    }
    System.out.println("==================================\n"+board);
    System.out.println("winner:"+board.isWin());
    
    if (board.isWin()==null)
      null
      else if (board.isWin()==playerA.getColor())
        playerA
        else playerB 
        
    
  }
  
 
    
 
  
  
}