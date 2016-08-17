class RecentOpponents (m: Map[String, Double], s: Map[(String, String), Double]) {
  def updated(u1: String, u2: String) : RecentOpponents =
    if (u1 > u2) updated (u2, u1)
    else {
      val s1 = m.get(u1).getOrElse(1.0)
      val s2 = m.get(u2).getOrElse(1.0)
      val x = s.get((u1, u2)).getOrElse(0.0)
      new RecentOpponents(m.updated(u1, s1 * 0.5).updated(u2, s2 * 0.5),
                          s.updated((u1, u2), x + s1 * s2))
    }
  def score(u1: String, u2: String) : Double =
    if (u1 > u2) score(u2, u1)
    else s.get((u1, u2)).getOrElse(0.0)
}

object RecentOpponents {
  def empty: RecentOpponents = new RecentOpponents (Map.empty[String, Double], Map.empty[(String, String), Double])
}
