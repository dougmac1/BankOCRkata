import org.scalatest.{MustMatchers, WordSpec}

object BankOCRSpec extends WordSpec with MustMatchers {

  "BankOCR" must {

    "return 1 when given two vertical pipes" in {
      BankOCR.convert("|\n|") mustEqual 1
    }
  }

}
