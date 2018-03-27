import org.scalatest.{MustMatchers, WordSpec}

class BankOCRSpec extends WordSpec with MustMatchers {

  "BankOCR" must {

    "return 1 when given two vertical pipes" in {
      val input = List(
        "   ",
        "  |",
        "  |",
        "   ").mkString("\n")
      BankOCR.convert(input) mustEqual "1"
    }

    "return 2 when representing 2 as pipes" in {
      val input = List(
        " _ ",
        " _|",
        "|_ ",
        "   ").mkString("\n")
      BankOCR.convert(input) mustEqual "2"
    }

    "return 3 when representing 3 as pipes" in {
      val input = List(
        " _ ",
        " _|",
        " _|",
        "   ").mkString("\n")
      BankOCR.convert(input) mustEqual "3"
    }

    "return 12 when given 12 as pipes" in {
      val input = List(
        "    _ ",
        "  | _|",
        "  ||_ ",
        "      ").mkString("\n")
      BankOCR.convert(input) mustEqual "12"
    }

    "return 000000000 when given nine 0's as pipes" in {
      val input = List(
          " _  _  _  _  _  _  _  _  _ ",
          "| || || || || || || || || |",
          "|_||_||_||_||_||_||_||_||_|",
          "                           ").mkString("\n")
      BankOCR.convert(input) mustEqual "000000000"
    }
  }
}
