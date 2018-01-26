object TestEloPerformance extends App {
  val eps = 1e-6
  def f(x:Double) = {
    printf("EloPerformance.perf(0, %.6f) = %.6f\n", x, EloPerformance.perf(0, x))
  }
  assert ((EloPerformance.perf(0, 50.0) - 0) < eps)
  assert ((EloPerformance.perf(0, 0.0) + 800.0) < eps)
  assert ((EloPerformance.perf(0, 100.0) - 800.0) < eps)
  assert ((EloPerformance.perf(0, 51.0) - 7) < eps)
  f(50.5)
  f(49.5)
  f(99.5)
}

