object BankOCR extends App {

  def convert(input: String): String = {

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


      val numberCount = split.head.length / 3
      val top = splitToCharBy3(split(0))
      val middle = splitToCharBy3(split(1))
      val bottom = splitToCharBy3(split(2))
      val output = for (eachNum <- 0 until numberCount) yield {
        numList(top(eachNum) + "\n" + middle(eachNum) + "\n" + bottom(eachNum) + "\n" + "   ")
      }
      output.mkString


    }

  def splitToCharBy3(row: String): List[String] = {
    "(...)".r.findAllIn(row).toList

    }
  }