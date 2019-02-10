package com.agynamix.garten.lib.field

import net.liftweb.record.field.StringTypedField
import net.liftweb.record.field.TimeZoneField
import net.liftweb.http.SHtml
import net.liftweb.common._
import net.liftweb.util.Helpers._
import net.liftweb.record.field.StringField
import java.util.TimeZone
import net.liftweb.record.Record
import net.liftweb.record.field.OptionalStringField
import scala.xml.NodeSeq
import org.joda.time.DateTimeZone

trait BsTimeZoneTypedField extends StringTypedField {
  /** Label for the selection item representing Empty, show when this field is optional. Defaults to the empty string. */
  def emptyOptionLabel: String = ""

  def buildDisplayList: List[(String, String)] =
      if (optional_?) ("", emptyOptionLabel)::TimeZoneField.timeZoneList else TimeZoneField.timeZoneList

  private def elem = SHtml.select(buildDisplayList, Full(valueBox openOr ""),
                                  timezone => setBox(Full(timezone)), "class" -> "form-control") % ("tabindex" -> tabIndex.toString)

  override def toForm: Box[NodeSeq] =
    uniqueFieldId match {
      case Full(id) => Full(elem % ("id" -> id))
      case _ => Full(elem)
    }
}

class BsTimeZoneField[OwnerType <: Record[OwnerType]](rec: OwnerType)
  extends StringField(rec, 32) with BsTimeZoneTypedField {

  override def defaultValue = TimeZone.getDefault.getID

  def isAsTimeZone: TimeZone = TimeZone.getTimeZone(value) match {
    case null => TimeZone.getDefault
    case x => x
  }

  def isAsJodaTimeZone: DateTimeZone = DateTimeZone.forID(value) match {
    case null => DateTimeZone.getDefault
    case x => x
  }

}

class OptionalBsTimeZoneField[OwnerType <: Record[OwnerType]](rec: OwnerType)
  extends OptionalStringField(rec, 32) with BsTimeZoneTypedField

