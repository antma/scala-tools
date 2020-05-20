import std.random;
import std.stdio;
import std.typecons;
import std.algorithm;

alias Edge = Tuple!(int, int, int);
alias P = Tuple!(int, int);

void header () {
  writeln (q"HEADER
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
HEADER");
}

void footer () {
  writeln (r"
    }
  }
}");
}

void gen (int vertices, int edges, int v) {
  if (edges == 0) return;
  Edge[] e;
  foreach (j; 1 .. vertices) foreach (i; 0 .. j) {
    e ~= Edge (i, j, -1);
  }
  e = partialShuffle (e, edges);
  foreach (i; 0 .. edges) {
    e[i][2] = uniform (0, v);
  }
  auto a = new int[][](vertices, vertices);
  foreach (const ref p; e) {
    a[p[0]][p[1]] = p[2];
  }
  auto dp = new P[1 << vertices];
  dp[] = P (-1, -1);
  P go (const int k) {
    //stderr.writeln ("k = ", k);
    if (dp[k][0] >= 0) return dp[k];
    auto res = P(0, 0);
    foreach (const ref p; e[0 .. edges]) {
      const int i = p[0];
      if (k & (1 << i)) continue;
      const int j = p[1];
      if (k & (1 << j)) continue;
      assert (i != j);
      assert (a[i][j] >= 0);
      auto w = go (k + (1 << i) + (1 << j));
      if (w[0] < 0) continue;
      ++w[0];
      w[1] -= a[i][j];
      if (res < w) res = w;
    }
    return dp[k] = res;
  }
  size_t k;
  writefln ("WMMatchingTest.check(%d, Array(", vertices);
  foreach (i; 0 .. vertices) foreach (j; i + 1 .. vertices) {
    write (a[i][j]);
    if (i < vertices - 2) write (", ");
    if (++k >= 15) {
      writeln;
      k = 0;
    }
  }
  auto res = go (0);
  res[1] *= -1;
  writefln ("), (%d, %d)) must beTrue", res[0], res[1]);
}

void checks () {
  rndGen.seed (20200521);
  gen (2, 1, 10);
  /*
  foreach (n; 2 .. 10) {
    foreach (p; [5, 10, 25, 50, 100]) {
      gen (n, (((n * (n - 1)) / 2) * p) / 100, 100);
    }
  }
  */
}

void main () {
  header ();
  checks ();
  footer ();
}
