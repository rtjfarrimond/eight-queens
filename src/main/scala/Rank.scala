opaque type Rank = Int

object Rank:
  val allRanks: List[Rank] = (1 to 8).map(identity).toList

  def fromInt(i: Int): Option[Rank] =
    if (allRanks.contains(i)) Some(i) else None

  extension(r: Rank)
    def predecessor: Option[Rank] =
      Rank.fromInt(r - 1)
    def successor: Option[Rank] =
      Rank.fromInt(r + 1)
