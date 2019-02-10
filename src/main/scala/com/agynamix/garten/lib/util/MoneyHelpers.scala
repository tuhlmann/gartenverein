package com.agynamix.garten.lib.util

import java.text.DecimalFormat
import java.text.NumberFormat
import org.joda.money.Money
import org.joda.money.format.MoneyFormatterBuilder

object MoneyHelpers extends MoneyHelpers

trait MoneyHelpers {

  def moneyToString(value: Money) = {
    val fmt = new MoneyFormatterBuilder().appendAmountLocalized().toFormatter()
    fmt.print(value)
  }

}