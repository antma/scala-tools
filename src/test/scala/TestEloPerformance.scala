package lila.common
import org.specs2.mutable.Specification

class EloPerformanceTest extends Specification {
  "elo performance" should {
    "perf" in {
      val eps = 1e-6
      ((EloPerformance.perf(0, 50.0) - 0) < eps) must beTrue
      ((EloPerformance.perf(0, 0.0) + 800.0) < eps) must beTrue
      ((EloPerformance.perf(0, 100.0) - 800.0) < eps) must beTrue
      ((EloPerformance.perf(0, 51.0) - 7) < eps) must beTrue
    }
  }
}
