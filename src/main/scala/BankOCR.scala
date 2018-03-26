object BankOCR extends App {

  def convert(input: String): Int = {

    val numList: Map[String, Int] = Map(
      List(
        "   ",
        "  |",
        "  |",
        "   ").mkString("\n") -> 1,
      List(
        " _ ",
        " _|",
        "|_ ",
        "   ").mkString("\n") -> 2,
      List(
        " _ ",
        " _|",
        " _|",
        "   ").mkString("\n") -> 3,
      List(
        "   ",
        "|_|",
        "  |",
        "   ").mkString("\n") -> 4,
      List(
        " _ ",
        "|_ ",
        " _|",
        "   ").mkString("\n") -> 5,
      List(
        " _ ",
        "|_ ",
        "|_|",
        "   ").mkString("\n") -> 6,
      List(
        " _ ",
        "  |",
        "  |",
        "   ").mkString("\n") -> 7,
      List(
        " _ ",
        "|_|",
        "|_|",
        "   ").mkString("\n") -> 8,
      List(
        " _ ",
        "|_|",
        " _|",
        "   ").mkString("\n") -> 9,
      List(
        " _ ",
        "| |",
        "|_|",
        "   ").mkString("\n") -> 0)

    val split = input.split("\n").toList

    def splitToCharBy3 (row: String): List[String] = {
      for (grid <- 0 until 6 by 3) yield {
        val chars = row.substring(grid, grid + 3)
        chars

      }
    }

    val top = splitToCharBy3(split(0))
    val middle = splitToCharBy3(split(1))
    val bottom = splitToCharBy3(split(2))



  }
}