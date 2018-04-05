import java.io.{BufferedWriter, File, FileWriter}
import scala.util.Try
import scala.io.Source

class ReadFile {
  def read(name: String) = {
    val buffer = Source.fromFile(s"./src/main/scala/$name")
    val fileContents = buffer.getLines().map(line =>
      line + "\n"
    ).mkString
    buffer.close()
    fileContents
  }
}

class WriteFile {

  def write(text: String): File = {
    val file = new File("./src/main/scala/Account Output.txt")
    val bufferedWriter = new BufferedWriter(new FileWriter(file))
    bufferedWriter.write(text)
    bufferedWriter.close()
    file
  }
}

object BankOCR extends App {

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
    println(parsed.map(x => guessValidAccounts(x)).mkString("\n"))
    parsed.map(x => guessValidAccounts(x)).mkString("\n")
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

  def guessPossibleNumbers(input: List[String]): List[List[String]] = {
    // Takes a list of the 9 number patterns and for each pattern
    for (eachNum <- input.indices.toList) yield {
      // Get the number patterns for numbers 0 to 9
      val numberPatterns = (0 to 9).toList.map(numMap)
      // Zip the 0 to 9 patterns against each input number pattern to get a list of list of tuples
      val compareWithInput = numberPatterns.map(x => input(eachNum).zip(x))
      // Map twice to get to the patterns then for each char if tuple 1 and tuple 2 are different
      // return a 1, otherwise return a 0 and sum them which gives the number of differences for each number
      val numOfChanges = compareWithInput.map(x => x.map(l => if (l._2 != l._1) 1 else 0).sum)
      // Zip them with their index to give tuples with the number they are representing
      val addNumbers = numOfChanges.zipWithIndex
      //  Filter where there is only 1 or less changes and return the numbers they represent
      val getOneChangeOrLess = addNumbers.filter(x => x._1 <= 1)
      // map the number to string which gives a list of possible numbers for each of the 9 numbers in the Acc No.
      getOneChangeOrLess.map(_._2.toString)
    }
  }

  def guessValidAccounts(input: List[String]): String = {
    // Takes a list of the 9 number patterns and gets all possible combinations for each number
    val allCombinations = guessPossibleNumbers(input)
    // Call the function to get all 9 digit variations of all possible numbers
    val possibleAccounts = getAccountCombinations(allCombinations).map(_.mkString)
    // get the original converted number to use as a comparison
    val comparison = convertToNumber(input)
    // filter the list of all possible account numbers by those that pass the checksum function
    val validOptions = possibleAccounts.filter(checkSum(_).length == 9)
    // Zip the valid options against the original converted number
    val compareChanges = validOptions.map(x => x.zip(comparison))
    // filter for where there is only one difference in the string of 9 numbers to the original numbers
    val filteredForOneChange = compareChanges.filter(x => x.count(x => x._2 != x._1) == 1)
    // Return all of the numbers that have only one number different
    val validNumbers = filteredForOneChange.map(x => x.map(x => x._1).mkString)
    validNumbers.length match {
      case 0 => checkSum(comparison)
      case 1 => validNumbers.head
      case _ => s"$comparison AMB ['${validNumbers.mkString("', '")}']"
    }
  }

  def getAccountCombinations(inputList: List[List[String]]): List[List[String]] = inputList match {
    // Get every combination of every valid number for all 9 numbers
    case Nil => List(Nil)
    case h :: _ => h.flatMap(i => getAccountCombinations(inputList.tail).map(i :: _))
  }

  writeFile.write(convert(readFile.read("Bank Account Numbers.txt")))
}