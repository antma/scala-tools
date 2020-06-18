package lila.tournament
import scala.util.{Success, Failure}
import org.specs2.mutable.Specification

object ArenaTournamentColorHistoryTest {
  def reduce(s: String): (Int, Int) = {
    val x = s.foldLeft(ArenaTournamentColorHistory()) { (acc, c) => c match {
      case 'W' => ArenaTournamentColorHistory(acc.incColor(1))
      case 'B' => ArenaTournamentColorHistory(acc.incColor(-1))
    }}
    (x.strike, x.balance)
  }
}

class ArenaTournamentColorHistoryTest extends Specification {
  "arena tournament color history" should {
     "reduce" in {
       ArenaTournamentColorHistoryTest.reduce("WWW") must_== (3, 3)
       ArenaTournamentColorHistoryTest.reduce("WWWB") must_== (-1, 2)
       ArenaTournamentColorHistoryTest.reduce("BBB") must_== (-3, -3)
       ArenaTournamentColorHistoryTest.reduce("BBBW") must_== (1, -2)
       ArenaTournamentColorHistoryTest.reduce("WWWBBB") must_== (-3, 0)
    }
    "couldPlay" in {
      ArenaTournamentColorHistoryTest.reduce("WWW").couldPlay(
        ArenaTournamentColorHistoryTest.reduce("WWW", 3) must beFalse
      ArenaTournamentColorHistoryTest.reduce("BBB").couldPlay(
        ArenaTournamentColorHistoryTest.reduce("BBB", 3) must beFalse
      ArenaTournamentColorHistoryTest.reduce("BB").couldPlay(
        ArenaTournamentColorHistoryTest.reduce("BB", 3) must beTrue
    }
  }
}
