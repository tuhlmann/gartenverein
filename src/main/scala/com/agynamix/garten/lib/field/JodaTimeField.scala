package com.agynamix.garten.lib.field

import net.liftweb.record.TypedField
import net.liftweb.common._
import net.liftweb.util.Helpers._
import java.util.Calendar
import java.util.Date
import net.liftweb.json.DefaultFormats
import net.liftweb.util.Helpers
import net.liftweb.http.S
import net.liftweb.http.S.SFuncHolder
import scala.xml.NodeSeq
import net.liftweb.http.js.JE._
import net.liftweb.json.JsonAST._
import net.liftweb.record.Record
import net.liftweb.record.Field
import net.liftweb.record.MandatoryTypedField
import net.liftweb.record.OptionalTypedField
import org.joda.time.DateTime

trait BsJodaTimeTypedField extends TypedField[DateTime] {
  private final def dateToJodaTime(d: Date): DateTime = {
    new DateTime(d)
  }

  val formats = new DefaultFormats {
    override def dateFormatter = Helpers.internetDateFormatter
  }

  def setFromAny(in : Any): Box[DateTime] = toDate(in).flatMap(d => setBox(Full(dateToJodaTime(d)))) or genericSetFromAny(in)

  def setFromString(s: String): Box[DateTime] = s match {
    case null|"" if optional_? => setBox(Empty)
    case null|"" => setBox(Failure(notOptionalErrorMessage))
    case other => setBox(tryo(dateToJodaTime(parseInternetDate(s))))
  }

  private def elem =
    S.fmapFunc(SFuncHolder(this.setFromAny(_))){funcName =>
      <input type={formInputType}
        name={funcName} class="form-control"
        value={valueBox.map(s => toInternetDate(s.toDate())) openOr ""}
        tabindex={tabIndex toString}/>
    }

  def toForm: Box[NodeSeq] =
    uniqueFieldId match {
      case Full(id) => Full(elem % ("id" -> id))
      case _        => Full(elem)
    }

  def asJs = valueBox.map(v => Str(formats.dateFormat.format(v.toDate()))) openOr JsNull

  def asJValue = asJString(v => formats.dateFormat.format(v.toDate()))
  def setFromJValue(jvalue: JValue) = setFromJString(jvalue) {
    v => formats.dateFormat.parse(v).map(d => {
      new DateTime(d)
    })
  }
}

class JodaTimeField[OwnerType <: Record[OwnerType]](rec: OwnerType)
  extends Field[DateTime, OwnerType] with MandatoryTypedField[DateTime] with BsJodaTimeTypedField {

  def owner = rec

  def this(rec: OwnerType, value: DateTime) = {
    this(rec)
    setBox(Full(value))
  }

  def defaultValue = new DateTime()
}

class OptionalJodaTimeField[OwnerType <: Record[OwnerType]](rec: OwnerType)
  extends Field[DateTime, OwnerType] with OptionalTypedField[DateTime] with BsJodaTimeTypedField {

  def owner = rec

  def this(rec: OwnerType, value: Box[DateTime]) = {
    this(rec)
    setBox(value)
  }
}
