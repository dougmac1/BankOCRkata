def splitToCharBy3 (row: String): IndexedSeq[String] = {
  for (grid <- 0 until 6 by 3) yield {
    val chars = row.substring(grid, grid + 3)
    chars
  }

}