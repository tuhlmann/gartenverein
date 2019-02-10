package com.agynamix.garten.model.share

object RogueAdditions {
  import java.util.Calendar
  import com.foursquare.rogue._
  import com.foursquare.rogue.LiftRogue._
  import net.liftweb.mongodb.record.BsonRecord
  import com.foursquare.field.{Field => RField}
  import org.joda.time.DateTime

  class CalendarQueryField[M](field: RField[Calendar, M])
    extends AbstractQueryField[Calendar, DateTime, java.util.Date, M](field) {
    override def valueToDB(d: DateTime) = d.toDate

    def before(d: DateTime) = new LtQueryClause(field.name, d.toDate)
    def after(d: DateTime) = new GtQueryClause(field.name, d.toDate)
    def onOrBefore(d: DateTime) = new LtEqQueryClause(field.name, d.toDate)
    def onOrAfter(d: DateTime) = new GtEqQueryClause(field.name, d.toDate)
  }

  class CalendarModifyField[M](field: RField[Calendar, M])
    extends AbstractModifyField[Calendar, java.util.Date, M](field) {
    override def valueToDB(c: Calendar) = c.getTime

    def setTo(d: DateTime) = new ModifyClause(ModOps.Set, field.name -> d.toDate)
  }

  implicit def calendarFieldToDateQueryField[M <: BsonRecord[M]]
    (f: net.liftweb.record.Field[Calendar, M]): CalendarQueryField[M] =
        new CalendarQueryField(f)

  implicit def calendarFieldToCalendarModifyField[M <: BsonRecord[M]]
    (f: net.liftweb.record.Field[Calendar, M]): CalendarModifyField[M] =
        new CalendarModifyField(f)

  implicit def rcalendarFieldToCalendarQueryField[M]
    (f: RField[Calendar, M]): CalendarQueryField[M] = new CalendarQueryField(f)

  implicit def rcalendarFieldToCalendarModifyField[M]
    (f: RField[Calendar, M]): CalendarModifyField[M] = new CalendarModifyField(f)
}