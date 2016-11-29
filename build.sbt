name := "ScalaTax"

version := "1.0"

scalaVersion := "2.11.7"

val taxes = inputKey[Unit]("Custom task for calculating taxes")

val taxesByYear = inputKey[Unit]("Custom task for calculating taxes")

fullRunInputTask(taxes, Compile, "scalatax.Taxes")

fullRunInputTask(taxesByYear, Compile, "scalatax.TaxesByYear")



    