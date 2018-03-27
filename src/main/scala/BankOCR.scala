import java.util.NoSuchElementException

object BankOCR extends App {

  def convert(input: String): String = {
try {
  val numList: Map[String, Int] = Map(

      "   " +
      "  |" +
      "  |" +
      "   " -> 1,

      " _ " +
      " _|" +
      "|_ " +
      "   " -> 2,

      " _ " +
      " _|" +
      " _|" +
      "   " -> 3,

       "   " +
      "|_|" +
      "  |" +
      "   " -> 4,

      " _ " +
      "|_ " +
      " _|" +
      "   " -> 5,

      " _ " +
      "|_ " +
      "|_|" +
      "   " -> 6,

      " _ " +
      "  |" +
      "  |" +
      "   " -> 7,

       " _ " +
      "|_|" +
      "|_|" +
      "   " -> 8,

       " _ " +
      "|_|" +
      " _|" +
      "   " -> 9,

      " _ " +
      "| |" +
      "|_|" +
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
} catch {
  case NoSuchElementException => "?"
}
  }

  def splitToCharBy3(row: String): List[String] = {
    "(...)".r.findAllIn(row).toList

  }


  def checkSum(accNo: String): String = {
    if (accNo.contains("?")) {
      accNo + " ILL"
    } else {
      val reverseString = accNo.reverse
      val numList = reverseString.map(_.toString.toInt).toList
      val sumOfList = numList.zipWithIndex.map(x => x._1 * (x._2 + 1)).sum
      sumOfList % 11 == 0 match {
        case true => accNo
        case false => accNo + " ERR"
      }
    }
  }
convert(
  "    _ \n" +
  "    _|\n" +
  "  ||_ \n" +
  "      ")
}