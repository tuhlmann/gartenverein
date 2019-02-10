package com.agynamix.garten.lib.field

import net.liftweb.mongodb.record.field.OptionalDateField
import net.liftweb.record.Record
import net.liftweb.mongodb.record.BsonRecord
import org.joda.time.DateTime
import org.joda.time.Years
import net.liftweb.common._
import com.agynamix.garten.lib.util.LocalizedEnum
import scala.xml.Text

object GenderType extends Enumeration with LocalizedEnum {
  type GenderType = Value

  def locKeyBase = "gender"

  val Male = Value("male")
  val Female = Value("female")

}

abstract class GenderField[OwnerType <: BsonRecord[OwnerType]](rec: OwnerType) extends BsEnumNameField(rec, GenderType) {
    override def displayName = "Geschlecht"

    override def buildDisplayList = GenderType.buildDisplayList(enum.values.toList)
    override def asHtml = GenderType.asHtml(get)

  }
