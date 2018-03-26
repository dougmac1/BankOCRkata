object BankOCR extends App {

  def convert(x: String): Int = {

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

    numList(x)

  }

  println(convert("""
         _
        |_|
         _|
        """.stripMargin))


}
