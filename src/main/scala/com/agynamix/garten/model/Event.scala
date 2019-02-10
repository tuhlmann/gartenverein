package com.agynamix.garten.model

import net.liftweb.common._
import net.liftweb.util.Helpers._
import com.agynamix.garten.model.share._
import com.agynamix.garten.lib.StructuredDbObject
import com.agynamix.garten.lib.field._
import scala.xml.Text
import java.util.Date
import net.liftweb.mongodb.BsonDSL._
import net.liftweb.record.LifecycleCallbacks
import org.bson.types.ObjectId
import com.foursquare.rogue.LiftRogue._
import org.joda.time.DateTime
import net.liftweb.mongodb.record.field.OptionalDateField
import com.agynamix.garten.snippet.EventModalDialog
import org.joda.time.DateMidnight
import net.liftweb.json.JsonAST.JObject
import net.liftweb.common.Full

object Event extends Event with LogbookMetaRecord[Event] with StructuredDbObject[Event] with FormValidators[Event] {

  /**
   * Ensure that static jobs exist, create them if necessary.
   * This is started from boot and runs at initialization time
   */
  def init(): Unit = {
    resetInProgressItems
  }

  def keyFields(user: User, args: Any*): Box[() => JObject] =
    user.activeMembership.activeClient.map(client => () => (clientId.name -> client.id.get))

  def createInstance(user: User, client: Client) = {
    Event.createRecord.author(user.id.get).clientId(client.id.get)
  }

  /**
   * Find the next item that has a past due date and should somehow be processed by the JobScheduler
   */
  def findNextDueItem: Box[Event] = {
    val timeNow = new DateTime()

    val re = Event where (_.reminder neqs ReminderType.None) and (_.calcReminderDate before timeNow) and (_.jobState eqs ScheduledJobState.Open) findAndModify
      (_.jobState setTo ScheduledJobState.InProgress) updateOne (true)

    if (re.isDefined)
      logger.info(s"Find next Event at ${timeNow}. Found: "+re.map(e => e.subject.get).getOrElse("Nothing found"))

    re
  }

  /**
   * Check all items and reset the state of this in progress- happens when the server crashes while
   * items being processed.
   */
  def resetInProgressItems(): Unit = {
    (Event where (_.jobState eqs ScheduledJobState.InProgress) fetch ()).foreach(_.jobState(ScheduledJobState.Open).save(true))
  }

  def findReminderRecipients(event: Event): List[RecipientData] = {
    event.recipients.obj.map(_.findEmailRecipients(event.author.get, event)).openOr(Nil)
  }

  def findAppointments(user: User, clientId: ObjectId, start: Date, end: Date): List[Event] = {
    Event where (_.clientId eqs clientId) and (_.startDate between (start, end)) orderAsc (_.startDate) fetch
  }

  def findDashboardEvents(user: User, clientId: ObjectId, start: Date, end: Date, limit: Int): List[Event] = {
    Event where (_.clientId eqs clientId) and (_.startDate between (start, end)) orderAsc (_.startDate) limit(limit) fetch
  }

}

class Event private () extends MongoIdRecord[Event] with ClientId[Event]
                       with CreatedUpdated[Event] with Attachments[Event]
                       with Schedulable[Event]
                       with Recipients[Event] with Loggable {
  def meta = Event

  def getClientId = this.clientId.get
  def uploadDoneCallback = EventModalDialog.uploadDoneCallback

  implicit def date2datetime(d: Date) = new DateTime(d, User.getDateTimeZone)
  implicit def datetime2date(d: DateTime) = d.toDate()
  implicit def dateMN2datetime(dmn: DateMidnight) = dmn.toDateTime()
  implicit def dateMN2date(dmn: DateMidnight) = dmn.toDate()

  object author extends BsObjectIdRefField(this, User) {
    override def displayName = "Autor"
    override def asHtml = Text(obj.map(_.displayName).openOr(""))
  }

  object startDate extends BsDateTimeField(this) with LifecycleCallbacks {
    override def displayName = "Anfang"
    override def defaultValue = {
      val dt: DateTime = new DateTime(User.getDateTimeZone)
      dt.withMinuteOfHour(0).plusHours(1).toDate()
    }
    override def beforeSave {
      if (owner.allDay.get) {
        this.set(get.toDateMidnight())
      }
    }
  }

  object endDate extends BsOptionalDateTimeField(this) with LifecycleCallbacks {
    override def displayName = "Ende"
    override def defaultValueBox = Full(new DateTime(owner.startDate.get).plusMinutes(30).toDate())
    override def beforeSave {
      if (owner.allDay.get) {
        val re = owner.startDate.get.plusDays(1).toDateMidnight().minusMinutes(1)
        println("End Date: "+re)
        this.setBox(Full(re))
      }
    }
  }

  object allDay extends EventAllDayBooleanField(this) {
    override def displayName = "Ganztägig"
    override def defaultValue = true
  }

  object calcReminderDate extends OptionalDateField(this) with LifecycleCallbacks {

    override def beforeSave {
      calcReminderDate.setBox(ReminderHelpers.calculateReminderDate(startDate.valueBox, reminder.get, reminderAlertAdvance.get))
      println("Reminder: "+valueBox.map(_.getTime()))
    }

  }

  object reminderAlertAdvance extends ReminderAlertAdvanceField(this)

  object reminder extends ReminderTypeField(this, reminderAlertAdvance)

  object subject extends BsStringField(this, 255) {
    override def validations = valMinLen(3, "Bitte geben Sie einen Betreff an.") _ :: super.validations
    override def displayName = "Betreff"
  }

  object note extends BsHtmlAreaField(this, 10000) {
    override def displayName = "Notiz"
  }


  /**
   * Reschedule (we may have multiple reminders in the future, or close reminder
   */
  def rescheduleOrDone: Boolean = {
    this.jobState(ScheduledJobState.Closed) /* do nothing */
    this.save(true)
    true
  }

}