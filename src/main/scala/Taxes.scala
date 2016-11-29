package scalatax

object Taxes extends Brackets with TaxCalculator {

  def main(args: Array[String]): Unit = {
    val options = TaxOptions.apply(args)
    println(options)
    println(calculate(options.agi, currentBracket(), options.netIncome))
  }

}

object TaxesByYear extends Brackets with TaxCalculator {

  def main(args: Array[String]): Unit = {
    val options = TaxOptions.apply(args)
    println(options)
    println(calculate(options.agi, currentBracket(options.year.toString), options.netIncome))
  }

}

