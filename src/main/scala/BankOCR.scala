import scala.util.Try

object BankOCR extends App {

  def parseAccounts(input: String): List[String] = {
    "(.+\n?){4}".r.findAllIn(input).toList
  }

  def parseDigits(input: String): List[String] = {
    val split = input.split("\n").toList
    val numberCount = split.head.length / 3
    val top = "(...)".r.findAllIn(split.head).toList
    val middle = "(...)".r.findAllIn(split(1)).toList
    val bottom = "(...)".r.findAllIn(split(2)).toList

    for (eachNum <- (0 until numberCount).toList) yield {
      top(eachNum) + middle(eachNum) + bottom(eachNum) + "   "
    }
  }

  def convert(input: String): String = {
    val parsedAccs = parseAccounts(input)
    val parsed = parsedAccs.map(x => parseDigits(x))
    val converted = parsed.map(x => convertToNumber(x))
    converted.map(x => checkSum(x)).mkString("\n")
  }

  def convertToNumber(listOfDigits: List[String]): String = {
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
    listOfDigits.map(digit => Try(numList(digit)) getOrElse "?").mkString
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

  println(convert(
        "    _  _     _  _  _  _  _ \n" +
        "  | _| _||_||_ |_   ||_||_|\n" +
        "  ||_  _|  | _||_|  ||_| _|\n" +
        "                           \n" +
        " _  _  _  _  _  _  _  _  _ \n" +
        "| || || || |  || || || || |\n" +
        "|_||_||_||_|  ||_||_||_||_|\n" +
        "                           "))
}