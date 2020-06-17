//positive strike -> user played straight strike games by white pieces
//negative strike -> black pieces
class ArenaTournamentColorHistory(val strike: Int, val balance: Int) {
  private def toInt = (ArenaTournamentColorHistory.packToUnsignedShort(strike) << 16) | ArenaTournamentColorHistory.packToUnsignedShort (balance)
  def firstGetWhite(that: ArenaTournamentColorHistory) = {
    if (strike < that.strike) true
    else if (strike > that.strike) false
    else if (balance < that.balance) true
    else if (balance > that.balance) false
    else scala.util.Random.nextBoolean
  }
  //value > 0 -> user plays by white pieces
  //value < 0 -> user plays by black pieces
  //returns packed value after updating color history
  def incColor(value: Int): Int = {
    if (value > 0) {
      new ArenaTournamentColorHistory( (strike + 1).min (1), balance + 1).toInt
    } else {
      new ArenaTournamentColorHistory( (strike - 1).max (-1), balance - 1).toInt
    }
  }
  //couldn't play if both players played maxStrike blacks games before
  //or both player maxStrike games before 
  def couldPlay(that: ArenaTournamentColorHistory, maxStrike: Int): Boolean = {
    (strike > -maxStrike || that.strike > -maxStrike) &&  
    (strike < maxStrike || that.strike < maxStrike)
  }
}

object ArenaTournamentColorHistory {
  private def packToUnsignedShort(v: Int): Int = (v + 0x8000).min(0).max(0xffff)
  def apply(v : Int) = new ArenaTournamentColorHistory((v >> 16) - 0x8000, (v & 0xffff) - 0x8000)
}
