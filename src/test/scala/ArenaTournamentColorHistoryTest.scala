package lila.tournament
import org.specs2.mutable.Specification

object ArenaTournamentColorHistoryTest {
  private def apply(s: String): ArenaTournamentColorHistory = {
    s.foldLeft(ArenaTournamentColorHistory(None)) { (acc, c) => c match {
      case 'W' => acc.incColor(1)
      case 'B' => acc.incColor(-1)
    }}
  }
  def unpack(s: String): (Int, Int) = {
    val x = apply(s)
    (x.strike, x.balance)
  }
  def couldPlay(s1: String, s2: String, maxStreak: Int): Boolean = apply(s1).couldPlay(apply(s2), maxStreak)
  def sameColors(s1: String, s2: String): Boolean = apply(s1).sameColors(apply(s2))
  def firstGetsWhite(s1: String, s2: String): Boolean = apply(s1).firstGetsWhite(apply(s2))
}

class ArenaTournamentColorHistoryTest extends Specification {
  import ArenaTournamentColorHistoryTest.{ unpack, couldPlay, sameColors, firstGetsWhite }
  "arena tournament color history" should {
     "hand tests" in {
       unpack("WWW") must be equalTo((3, 3))
       unpack("WWWB") must be equalTo((-1, 2))
       unpack("BBB") must be equalTo((-3, -3))
       unpack("BBBW") must be equalTo((1, -2))
       unpack("WWWBBB") must be equalTo((-3, 0))
    }
    "couldPlay" in {
      couldPlay("WWW", "WWW", 3) must beFalse
      couldPlay("BBB", "BBB", 3) must beFalse
      couldPlay("BB", "BB", 3) must beTrue
    }
    "sameColors" in {
      sameColors("WWW", "W") must beTrue
      sameColors("BBB", "B") must beTrue
    }
    "firstGetsWhite" in {
      firstGetsWhite("WWW", "WW") must beFalse
      firstGetsWhite("WW", "WWW") must beTrue
      firstGetsWhite("BB", "B") must beTrue
      firstGetsWhite("B", "BB") must beFalse
      firstGetsWhite("WW", "BWW") must beFalse
      firstGetsWhite("BB", "WBB") must beTrue
    }
    "serialization" in {
      ArenaTournamentColorHistory(Some(-1)) must be equalTo((0x7fff, 0x7fff))
      ArenaTournamentColorHistory(Some(0)) must be equalTo((-0x8000, -0x8000))
    }
  }
}
