package scalatax

trait TaxCalculator {

  def calculate(agi: Float, brackets: Vector[TaxBracket], netIncome: Float): TaxSummary = {
    TaxSummary(
      brackets.foldLeft(0f) { (sum, bracket) =>
        bracket.calculate(agi) match {
          case Some(b) =>
            println(b)
            sum + b.total
          case None => sum
        }
      }, netIncome
    )
  }

}
