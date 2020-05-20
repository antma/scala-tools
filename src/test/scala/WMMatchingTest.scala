package lila.common
//import lila.common.WMMatching
import scala.util.{Try, Success, Failure}
import org.specs2.mutable.Specification

object WMMatchingTest {
  def check(n: Int, a: Array[Int], res: (Int, Int)): Boolean = {
    val v = Array.range(0, n)
    def f(x: Int) = (x * (x - 1)) / 2
    def off(i: Int) = f(n - 1) - f(n - i)
    def pairScore(i: Int, j: Int): Option[Int] = {
      if(i > j) pairScore(j, i)
      else {
        val o = off(i) + (j - (i + 1))
        if (a(o) < 0) None else Some(a(o))
      }
    }
    def score(l: List[(Int, Int)]): (Int, Int) = (l.length, l.map (t => pairScore(t._1, t._2).head).sum)
    val m = WMMatching(v, pairScore)
    m match {
      case Success(l) => score(l) == res
      case Failure(e) => false
    }
  }
}

class WMMatchingTest extends Specification {
  "precomputed tests" should {
     "data" in {

WMMatchingTest.check(2, Array(
7), (1, 7)) must beTrue

    }
  }
}
