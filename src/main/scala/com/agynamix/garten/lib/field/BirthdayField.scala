package com.agynamix.garten.lib.field

import net.liftweb.mongodb.record.field.OptionalDateField
import net.liftweb.record.Record
import net.liftweb.mongodb.record.BsonRecord
import org.joda.time.DateTime
import org.joda.time.Years
import net.liftweb.mongodb.record.field.DateField

abstract class OptionalBirthdayField[OwnerType <: BsonRecord[OwnerType]](rec: OwnerType)
               extends OptionalDateField(rec) with DateFormAdapterField with GermanDateField {

  override def displayName = "Geburtstag"

  def currentAge: String = {
    (for (v <- valueBox) yield {
      val birthdate = new DateTime(v)
      val now = new DateTime()
      val age = Years.yearsBetween(birthdate, now).getYears()
      s"${age.toString()} ${if (age < 2) "Jahr" else "Jahre"}"
    }) openOr ""
  }

}

abstract class BirthdayField[OwnerType <: BsonRecord[OwnerType]](rec: OwnerType)
               extends DateField(rec) with DateFormAdapterField with GermanDateField {

  override def displayName = "Geburtstag"

  def currentAge: String = {
    (for (v <- valueBox) yield {
      val birthdate = new DateTime(v)
      val now = new DateTime()
      val age = Years.yearsBetween(birthdate, now).getYears()
      s"${age.toString()} ${if (age < 2) "Jahr" else "Jahre"}"
    }) openOr ""
  }

}
