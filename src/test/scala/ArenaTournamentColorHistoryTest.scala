package lila.tournament
import org.specs2.mutable.Specification

object ArenaTournamentColorHistoryTest {
  def apply(s: String): ArenaTournamentColorHistory = {
    s.foldLeft(ArenaTournamentColorHistory(None)) { (acc, c) => c match {
      case 'W' => acc.incColor(1)
      case 'B' => acc.incColor(-1)
    }}
  }
  def unpack(s: String): (Int, Int) = {
    val x = apply(s)
    (x.strike, x.balance)
  }
}

//must be equalTo

class ArenaTournamentColorHistoryTest extends Specification {
  "arena tournament color history" should {
     "hand tests" in {
       ArenaTournamentColorHistoryTest.unpack("WWW") must be equalTo((3, 3))
       ArenaTournamentColorHistoryTest.unpack("WWWB") must be equalTo((-1, 2))
       ArenaTournamentColorHistoryTest.unpack("BBB") must be equalTo((-3, -3))
       ArenaTournamentColorHistoryTest.unpack("BBBW") must be equalTo((1, -2))
       ArenaTournamentColorHistoryTest.unpack("WWWBBB") must be equalTo((-3, 0))
    }
    "couldPlay" in {
      ArenaTournamentColorHistoryTest("WWW").couldPlay(
        ArenaTournamentColorHistoryTest("WWW"), 3) must beFalse
      ArenaTournamentColorHistoryTest("BBB").couldPlay(
        ArenaTournamentColorHistoryTest("BBB"), 3) must beFalse
      ArenaTournamentColorHistoryTest("BB").couldPlay(
        ArenaTournamentColorHistoryTest("BB"), 3) must beTrue
    }
    "sameColors" in {
      ArenaTournamentColorHistoryTest("WWW").sameColors(
        ArenaTournamentColorHistoryTest("W")) must beTrue
      ArenaTournamentColorHistoryTest("B").sameColors(
        ArenaTournamentColorHistoryTest("BBB")) must beTrue
    }
  }
}
