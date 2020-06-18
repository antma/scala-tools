package lila.common

object EloPerformance {
  //FIDE Title Regulations effective from 1 July 2014 till 30 June 2017
  //https://www.fide.com/component/handbook/?id=174&view=article
  val tbl:Array[Int] = Array(-800, -677, -589, -538, -501, -470, -444, -422, -401, -383, -366, -351, -336, -322, -309, -296, -284, -273, -262, -251, -240, -230,
    -220, -211, -202, -193, -184, -175, -166, -158, -149, -141, -133, -125, -117, -110, -102, -95, -87, -80, -72, -65, -57, -50, -43, -36, -29, -21, -14, -7,
    0, 7, 14, 21, 29, 36, 43, 50, 57, 65, 72, 80, 87, 95, 102, 110, 117, 125, 133, 141, 149, 158, 166, 175, 184, 193, 202, 211, 220, 230,
    240, 251, 262, 273, 284, 296, 309, 322, 336, 351, 366, 383, 401, 422, 444, 470, 501, 538, 589, 677, 800)
  def perf(opponent_average_rating: Double, percent: Double) = {
    val x1 = if (percent < 1) 1 else if (percent > 99) 99 else (percent + 0.5).toInt
    val y1 = tbl(x1)
    val x0 = x1 - 1
    val y0 = tbl(x0)
    val x2 = x1 + 1
    val y2 = tbl(x2)
    //https://en.wikipedia.org/wiki/Lagrange_polynomial
    opponent_average_rating + 0.5 * (y0 * (percent - x1) * (percent - x2) + y2 * (percent - x0) * (percent - x1)) - y1 * (percent - x0) * (percent - x2)
  }
}
