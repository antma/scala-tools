//import WMMatching
import scala.util.Random

object PruneMatcher {
  type Score = Int
  type RankedPlayer = Int
  type WeightedPairing = (RankedPlayer, RankedPlayer, Int)
  type Combination = (Score, List[(RankedPlayer, RankedPlayer)])

  def f (x: List[WeightedPairing], cur: Combination, best: Combination, remaining_pairs: Int) : Combination = {
    if (remaining_pairs == 0) cur
    else {
      val (_, _, t) = (x take remaining_pairs).unzip3
      if (t.size < remaining_pairs || t.sum + cur._1 >= best._1) best
      else {
        val p = x.head
        val include_p = f ( x.tail.filter { q => p._1 != q._1 && p._1 != q._2 && p._2 != q._1 && p._2 != q._2 },
                           (cur._1 + p._3, (p._1, p._2) :: cur._2),
                           best,
                           remaining_pairs - 1)
        // exclude_p
        f (x.tail, cur, include_p, remaining_pairs)
      }
    }
  }
  def minWeightMatching (edges: List[(Int, Int, Int)]) : Array[Int] = {
    val nvertex = 1 + edges.map ( x => x._1.max (x._2) ).max
    assert((nvertex % 2) == 0)
    val c = f(edges.sortWith((a, b) => a._3 < b._3), (0, Nil), (Int.MaxValue, Nil), nvertex >> 1)
    val a: Array[Int] = Array.fill(nvertex)(-1)
    for ((u, v) <- c._2) {
      assert (u != v)
      assert (u >= 0 && u < nvertex)
      assert (v >= 0 && v < nvertex)
      assert (a(u) == -1 && a(v) == -1)
      a(u) = v
      a(v) = u
    }
    a
  }
}

object Checker {
  def result(edges: List[(Int, Int, Int)], a: Array[Int], s: Int): Int = {
    edges match {
      case Nil => s
      case x :: xs => result (xs, a, s + (if (a(x._1) == x._2) x._3 else 0))
    }
  }
  def check(edges: List[(Int, Int, Int)], a: Array[Int], b: Array[Int] ) = {
    assert (a.length == b.length)
    val ra = result(edges, a, 0)
    printf ("prune solution %d, %s\n", ra, a.toList)
    val rb = result(edges, b, 0)
    printf ("matcher solution %d, %s\n", rb, b.toList)
    assert(ra == rb)
  }
}

object TestWMMatching extends App {
  def test(n: Int, min_weight: Int, max_weight: Int, seed: Int) {
    printf("test(n: %d, min_weight: %d, max_weight: %d, seed: %d)\n", n, min_weight, max_weight, seed)
    val r = new Random(seed)
    val edges = (for (j <- 1 until n; i <- 0 until j) yield (i, j, r.nextInt(max_weight-min_weight)+min_weight)).toList
    //println (edges)
    val a = PruneMatcher.minWeightMatching(edges)
    val b = WMMatching.minWeightMatching(edges)
    Checker.check(edges, a, b)
  }

  test(4, 1, 500, 12)
  //test(4, 1, 10, 7)
  //test (4, 1, 10, 27)
  //test(4, 1, 20, 9)

  var seed = 1
  val iterations = 1000

  for (n <- 2 to 20 by 2;
       t <- List(10, 20, 50, 100, 200, 500, 1000, 10000);
       i <- 1 to iterations) {
    test(n, 1, t, seed)
    seed += 1
  }

  /*
  test (2, 1, 10, 1)
  test (4, 1, 5, 1)
  test (6, 1, 10, 1)
  */

}
