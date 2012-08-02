package connect4

/**
 * enumeration for the color of tokens
 */
object Color extends Enumeration {
    type Color=Value
    val Circle,Cross=Value
    
    def toString(color:Value)=color match{
      case Cross => "x"
      case Circle => "o"
    }
    def invert(color:Value)=color match{
      case Cross => Circle
      case Circle => Cross
      case null =>null
    }
  }
 

  