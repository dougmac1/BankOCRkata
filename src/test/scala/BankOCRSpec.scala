import org.scalatest.{MustMatchers, WordSpec}

class BankOCRSpec extends WordSpec with MustMatchers {

  "BankOCR" must {

    "return 1 when given two vertical pipes" in {
      val input =
        """
          |
          |

        """.stripMargin
      BankOCR.convert(input) mustEqual 1
    }

    "return 2 when representing 2 as pipes" in {
      val input =
        """
           _
           _|
          |_

        """.stripMargin
      BankOCR.convert(input) mustEqual 2
    }

    "return 3 when representing 3 as pipes" in {
      val input =
        """
           _
           _|
           _|

        """.stripMargin
      BankOCR.convert(input) mustEqual 3
    }
  }

}
