package lila.tournament
import scala.util.{Success, Failure}
import org.specs2.mutable.Specification

object ArenaTournamentColorHistoryTest {
  def apply(s: String): (Int, Int) = {
    val x = s.foldLeft(ArenaTournamentColorHistory(None)) { (acc, c) => c match {
      case 'W' => acc.incColor(1)
      case 'B' => acc.incColor(-1)
    }}
    (x.strike, x.balance)
  }
}

class ArenaTournamentColorHistoryTest extends Specification {
  "arena tournament color history" should {
     "hand tests" in {
       ArenaTournamentColorHistoryTest("WWW") must_== (3, 3)
       ArenaTournamentColorHistoryTest("WWWB") must_== (-1, 2)
       ArenaTournamentColorHistoryTest("BBB") must_== (-3, -3)
       ArenaTournamentColorHistoryTest("BBBW") must_== (1, -2)
       ArenaTournamentColorHistoryTest("WWWBBB") must_== (-3, 0)
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
