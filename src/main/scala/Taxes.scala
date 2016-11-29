package scalatax

case class TaxBracket(percentage: Float, range: Range) {
  def by100 = percentage / 100

  def topOfRange = range.last

  def bottomOfRange = range.head

  def agiIsGreaterThanTop(agi: Float): Boolean = agi > topOfRange

  def agiIsGreaterThanBottom(agi: Float): Boolean = agi > bottomOfRange

  def difference(top: Float = topOfRange, bottom: Float = bottomOfRange) = top - bottom

  def calculate(agi: Float): Option[TaxBracketSummary] = {
    (agiIsGreaterThanTop(agi), agiIsGreaterThanBottom(agi)) match {
      case (true, _) =>
          TaxBracketSummary(
            top = topOfRange,
            bottom = bottomOfRange,
            percentage = percentage,
            total = totalTax(difference())
          ).toOption
      case (false, true) =>
          TaxBracketSummary(
            top = topOfRange,
            bottom = bottomOfRange,
            percentage = percentage,
            total = totalTax(difference(agi))
          ).toOption
      case (false, _) => None
    }
  }

  private def totalTax(difference: Float) = difference * by100

}

case class TaxBracketSummary(top: Float, bottom: Float, percentage: Float, total: Float) {
  def toOption = Some(this)

  override def toString = s"Tax Bracket ($bottom - $top) $percentage%: $total"
}

case class TaxOptions(year: Int = 2016, netIncome: Float, estimatedDeductions: Float) {
  override def toString = s"Tax Year: $year\nNet Income: $netIncome\nEstimated Deductions: $estimatedDeductions"

  def agi = netIncome - estimatedDeductions
}

object TaxOptions {
  def apply(args: Array[String]): TaxOptions = TaxOptions(2016, args(0).toFloat, args(1).toFloat)
}

trait Brackets {
  private val brackets2016 = Vector (
    TaxBracket(10, 0 to 9275),
    TaxBracket(15, 9275 to 37650),
    TaxBracket(25, 37650 to 91150),
    TaxBracket(28,  91150 to 190150),
    TaxBracket(33, 190150 to 413350),
    TaxBracket(35, 413350 to 415050),
    TaxBracket(39.6f, 415050 to Int.MaxValue)
  )

  val brackets = Map("2016" -> brackets2016)

  def currentBracket(year: String = "2016"): Vector[TaxBracket] = brackets.find(_._1 == year).map(_._2)
                                                                          .getOrElse(brackets.head._2)
}

trait TaxCalculator {
  def calculate(agi: Float, brackets: Vector[TaxBracket]) = {

    brackets.foldLeft(0d) { (sum, bracket) =>
      bracket.calculate(agi) match {
        case Some(b) =>
          println(b)
          sum + b.total
        case None => sum
      }
    }
  }
}

object Taxes extends Brackets with TaxCalculator {

  def main(args: Array[String]): Unit = {
    val options = TaxOptions.apply(args)
    println(options)
    println(s"Income Tax Estimate: ${calculate(options.agi, currentBracket())}")
  }

}

object TaxesByYear extends Brackets with TaxCalculator {

  def main(args: Array[String]): Unit = {
    val options = TaxOptions.apply(args)
    println(options)
    println(s"Income Tax Estimate: ${calculate(options.agi, currentBracket(options.year.toString))}")
  }

}

