package scalatax

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
