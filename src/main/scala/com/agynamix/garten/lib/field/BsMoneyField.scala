package com.agynamix.garten.lib.field

import scala.xml._
import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.http.S
import net.liftweb.json._
import net.liftweb.record._
import net.liftweb.record.field._
import S._
import Helpers._
import net.liftmodules.extras.SnippetHelper
import java.text.DecimalFormat
import com.agynamix.garten.lib.util.MoneyHelpers
import org.joda.money.Money
import org.joda.money.format.MoneyFormatterBuilder
import org.joda.money.CurrencyUnit
import net.liftweb.http.js.JE.Str
import net.liftweb.http.js.JE.JsNull

/**
 * Feld für Geldbeträge.
 * Geldbetrag wird als cent betrag al long gespeichert
 */


trait BsMoneyTypedField extends TypedField[Money] with SnippetHelper with Loggable {
  val maxLength: Int = 200

  def isAutoFocus: Boolean = false
  def maxLen = maxLength

  def defaultValue = Money.zero(defaultCurrency)
  def defaultCurrency = CurrencyUnit.EUR

  lazy val symbolDisplayFormatter = new MoneyFormatterBuilder().appendAmountLocalized().appendCurrencySymbolLocalized().toFormatter()
  lazy val displayFormatter       = new MoneyFormatterBuilder().appendAmountLocalized().toFormatter()
  lazy val displayParser          = new MoneyFormatterBuilder().appendCurrencyCode().appendAmountLocalized().toFormatter()
  lazy val fullFormatter          = new MoneyFormatterBuilder().appendCurrencyCode().appendAmount().toFormatter()

  def setFromAny(in: Any): Box[Money] = in match {
    case str: String if str.trim.nonEmpty => setFromString(str)
    case seq: Seq[_] if !seq.isEmpty => setFromAny(seq.head)
    case _ => genericSetFromAny(in)
  }

  def setFromString(s: String): Box[Money] = s match {
    case null|"" if optional_? => setBox(Empty)
    case null|"" => setBox(Failure(notOptionalErrorMessage))
    case s => setBox(display2money(s))
  }

  def asJs = valueBox.map(m => Str(money2db(m))) openOr JsNull

  def asJValue: JValue = valueBox.map(v => JString(money2db(v))) openOr (JNothing: JValue)
  def setFromJValue(jvalue: JValue): Box[MyType] = jvalue match {
    case JNothing|JNull if optional_? => setBox(Empty)
    case JString(s)                   => setFromString(s)
    case other                        => setBox(FieldHelpers.expectedA("JString", other))
  }

  def display2money(in: String): Box[Money] = {
    val str = {
      if ((in.nonEmpty) && (Character.isLetter(in.charAt(0)))) in else defaultCurrency.getCode()+in
    }
    val re = tryo(Money.parse(str)) or tryo(displayParser.parseMoney(str))
    logger.debug("Try to parse: "+str+": "+re)
    re
  }

  def money2display(m: Money): String = {
    displayFormatter.print(m)
  }

  def money2display(m: Box[Money]): String = {
    m.map(displayFormatter.print) openOr ""
  }

  def money2displayWithSymbol(m: Money): String = {
    symbolDisplayFormatter.print(m)
  }

  def money2db(m: Money): String = {
    fullFormatter.print(m)
  }

  private def elem = {
    val fieldId: String = uniqueFieldId.openOr(randomString(12))

    S.fmapFunc(SFuncHolder(setFromAny(_))) {
      funcName =>
        <input type={formInputType}
          id={fieldId}
          name={funcName}
          value={(money2display(valueBox)).toString}
          tabindex={tabIndex.toString}
          class="form-control" /> % autofocus(isAutoFocus)
    }
  }

  override def toForm: Box[NodeSeq] = Full(elem)

  override def asHtml = Text(money2display(valueBox))

}

class MoneyField[OwnerType <: Record[OwnerType]](rec: OwnerType)
  extends Field[Money, OwnerType] with MandatoryTypedField[Money] with BsMoneyTypedField {

  def this(rec: OwnerType, value: Money) = {
    this(rec)
    set(value)
  }

  def owner = rec

  protected def valueTypeToBoxString(in: ValueType): Box[String] = Full(money2db(in))
  protected def boxStrToValType(in: Box[String]): ValueType = in.flatMap(s => display2money(s)).openOr(defaultValue)
}

class OptionalMoneyField[OwnerType <: Record[OwnerType]](rec: OwnerType)
  extends Field[Money, OwnerType] with MandatoryTypedField[Money] with BsMoneyTypedField {

  override def optional_? = true

  def this(rec: OwnerType, value: Money) = {
    this(rec)
    set(value)
  }

  def owner = rec

  protected def valueTypeToBoxString(in: ValueType): Box[String] = Full(money2db(in))
  protected def boxStrToValType(in: Box[String]): ValueType = in.flatMap(s => display2money(s)).openOr(defaultValue)
}


class BsMoneyField[OwnerType <: Record[OwnerType]](rec: OwnerType)
  extends MoneyField(rec) with BsMoneyTypedField

class OptionalBsMoneyField[OwnerType <: Record[OwnerType]](rec: OwnerType)
  extends OptionalMoneyField(rec) with BsMoneyTypedField
