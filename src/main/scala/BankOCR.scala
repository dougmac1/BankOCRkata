object BankOCR extends App {

  def convert(input: String): String = {

    val numList: Map[String, Int] = Map(

        "   "+
        "  |"+
        "  |"+
        "   " -> 1,

        " _ "+
        " _|"+
        "|_ "+
        "   " -> 2,

        " _ "+
        " _|"+
        " _|"+
        "   "-> 3,

        "   "+
        "|_|"+
        "  |"+
        "   "-> 4,

        " _ "+
        "|_ "+
        " _|"+
        "   " -> 5,

        " _ "+
        "|_ "+
        "|_|"+
        "   " -> 6,

        " _ "+
        "  |"+
        "  |"+
        "   " -> 7,

        " _ "+
        "|_|"+
        "|_|"+
        "   " -> 8,

        " _ "+
        "|_|"+
        " _|"+
        "   " -> 9,

        " _ "+
        "| |"+
        "|_|"+
        "   " -> 0)

    val split = input.split("\n").toList


      val numberCount = split.head.length / 3
      val top = splitToCharBy3(split(0))
      val middle = splitToCharBy3(split(1))
      val bottom = splitToCharBy3(split(2))
      val output = for (eachNum <- 0 until numberCount) yield {
        numList(top(eachNum) + middle(eachNum) + bottom(eachNum) + "   ")
      }
      output.mkString


    }

  def splitToCharBy3(row: String): List[String] = {
    "(...)".r.findAllIn(row).toList

    }
  }