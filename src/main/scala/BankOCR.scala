import java.io.{BufferedWriter, File, FileWriter}
import scala.util.Try
import scala.io.Source

object ReadFile {
  def read(name : String) = {
    val buffer = Source.fromFile(s"/home/digital030585/TrainingFiles/BankOCRkata/src/main/scala/$name")
    val fileContents = buffer.getLines().map(line =>
      line + "\n"
    ).mkString
    buffer.close()
    fileContents
  }
}

object WriteFile {

  def write(text: String) : File = {
    val file = new File("/home/digital030585/TrainingFiles/BankOCRkata/src/main/scala/Account Output.txt")
    val bufferedWriter = new BufferedWriter(new FileWriter(file))
    bufferedWriter.write(text)
    bufferedWriter.close()
    file
  }
}

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
  WriteFile.write(convert(ReadFile.read("Bank Account Numbers.txt")))
}