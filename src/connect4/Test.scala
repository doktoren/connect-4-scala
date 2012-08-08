package connect4

object Test {
  def main(args : Array[String]) : Unit = {
    val base = new BaseBoard(7, 6)
    var board = new BitBoard(base)
    //val actions = "33333344552u12244uu06"
    val actions = "3333332424242"
    actions.foreach( action => {
      action match {
        case 'u' => board.undo_move()
        case _ => board.make_move(action - '0')
      }
      System.out.println(board.toString())
      if (board.game_has_ended())
        System.out.println("GAME HAS ENDED!")
      else
        System.out.println("Board evaluation for player to move is %d".format(board.eval_score()))
        System.out.println("Valid moves: " + board.list_moves().map(String.valueOf).reduce(_ + ", " + _))
    })
    if (false)
      base.patterns_per_square.foreach( pattern_list => {
        val union = pattern_list./:\(0L)(_|_)
        System.out.println(base.toString(b => if ((b & union) == 0) ' ' else 'X'))
      })
  }
}
