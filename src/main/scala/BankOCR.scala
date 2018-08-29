import java.io.{BufferedWriter, File, FileWriter}
import scala.util.Try
import scala.io.Source

class ReadFile {
  def read(name : String) = {
    val buffer = Source.fromFile(s"./src/main/scala/$name")
    val fileContents = buffer.getLines().map(line =>
      line + "\n"
    ).mkString
    buffer.close()
    fileContents
  }
}

class WriteFile {

  def write(text: String) : File = {
    val file = new File("./src/main/scala/Account Output.txt")
    val bufferedWriter = new BufferedWriter(new FileWriter(file))
    bufferedWriter.write(text)
    bufferedWriter.close()
    file
  }
}

object BankOCRTest extends App {

  val readFile = new ReadFile
  val writeFile = new WriteFile

  lazy val numList: Map[String, Int] = Map(

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

  lazy val numMap = numList.map(_.swap)

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
    println(parsed.map(x => guessNumbers(x)).mkString("\n"))
    parsed.map(x => guessNumbers(x)).mkString("\n")
  }

  def convertToNumber(listOfDigits: List[String]): String = {
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

  def guessNumbers(input: List[String]): String = {
    val allCombinations = for (eachNum <- input.indices.toList) yield {
      val numberPatterns = (0 to 9).toList.map(numMap)
      val compareWithInput = numberPatterns.map(x => input(eachNum).zip(x))
      val numOfChanges = compareWithInput.map(x => x.map(l => if (l._2 != l._1) 1 else 0).sum)
      val addNumbers = numOfChanges.zipWithIndex
      val getOneChangeOrLess = addNumbers.filter(x => x._1 <= 1)
      val returnMatches = getOneChangeOrLess.map(_._2.toString)
      returnMatches
    }

    val possibleAccounts = getAccountCombinations(allCombinations).map(_.mkString)
    val comparison = convertToNumber(input)
    val validOptions = possibleAccounts.filter(checkSum(_).length == 9)
    val compareChanges = validOptions.map(x => x.zip(comparison))
    val filteredForOneChange = compareChanges.filter(x => x.count(x => x._2 != x._1) ==1 )
    val validNumbers = filteredForOneChange.map(x => x.map(x => x._1).mkString)
    validNumbers.length match {
      case 0 => checkSum(comparison)
      case 1 => validNumbers.head
      case _ => s"$comparison AMB ['${validNumbers.mkString("', '")}']"
    }
  }

  def getAccountCombinations(inputList: List[List[String]]): List[List[String]] = inputList match {
    case Nil => List(Nil)
    case h :: _ => h.flatMap(i => getAccountCombinations(inputList.tail).map(i :: _))
  }

  writeFile.write(convert(readFile.read("Bank Account Numbers.txt")))
}