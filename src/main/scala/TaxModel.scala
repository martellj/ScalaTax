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

case class TaxSummary(estimatedTaxes: Float, netIncome: Float) {
  def effectiveRate = s"${(estimatedTaxes / netIncome) * 100}%"

  override def toString = s"Tax Estimate: $estimatedTaxes\nEffective Rate: $effectiveRate"
}