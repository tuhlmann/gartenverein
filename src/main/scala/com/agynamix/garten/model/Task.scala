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
import com.foursquare.rogue.LiftRogue._
import org.joda.time.DateTime
import net.liftweb.mongodb.record.field.DateField
import net.liftweb.mongodb.record.field.OptionalDateField
import com.agynamix.garten.snippet.TaskModalDialog
import net.liftweb.json.JsonAST.JObject
import net.liftweb.common.Full
import org.bson.types.ObjectId


object Task extends Task with LogbookMetaRecord[Task] with StructuredDbObject[Task] with FormValidators[Task] {

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
    Task.createRecord.author(user.id.get).clientId(client.id.get)
  }

  /**
   * Find the next item that has a past due date and should somehow be processed by the JobScheduler
   */
  def findNextDueItem: Box[Task] = {
    val timeNow = new DateTime()

    val re = Task where (_.reminder neqs ReminderType.None) and (_.calcReminderDate before timeNow) and (_.jobState eqs ScheduledJobState.Open) findAndModify
      (_.jobState setTo ScheduledJobState.InProgress) updateOne (true)

    if (re.isDefined)
      logger.info(s"Find next Task at ${timeNow}. Found: "+re.map(e => e.subject.get).getOrElse("Nothing found"))

    re
  }

  /**
   * Check all items and reset the state of this in progress- happens when the server crashes while
   * items being processed.
   */
  def resetInProgressItems(): Unit = {
    (Task where (_.jobState eqs ScheduledJobState.InProgress) fetch ()).foreach(_.jobState(ScheduledJobState.Open).save(true))
  }

  def findReminderRecipients(task: Task): List[RecipientData] = {
    task.recipients.obj.map(_.findEmailRecipients(task.author.get, task)).openOr(Nil)
  }

  def findDashboardTasks(user: User, clientId: ObjectId, start: Date, end: Date, limit: Int): List[Task] = {
    Task where (_.clientId eqs clientId) and (_.dueDate between (start, end)) orderAsc (_.dueDate) limit(limit) fetch
  }


}

class Task private () extends MongoIdRecord[Task] with ClientId[Task]
                      with CreatedUpdated[Task] with Attachments[Task]
                      with Schedulable[Task]
                      with Recipients[Task] with Loggable {
  def meta = Task

  def getClientId = this.clientId.get
  def uploadDoneCallback = TaskModalDialog.uploadDoneCallback

  object author extends BsObjectIdRefField(this, User) {
    override def displayName = "Autor"
    override def asHtml = Text(obj.map(_.displayName).openOr(""))
  }

  object noteCreated extends DateField(this) with DateFormAdapterField with GermanDateField {
    override def displayName = "Erstellt am"
    override def defaultValue = new Date()
  }

  object dueDate extends BsOptionalDateTimeField(this) {
    override def displayName = "Fällig am"
    override def defaultValueBox = {
      val dt: DateTime = new DateTime(User.getDateTimeZone)
      Full(dt.withMinuteOfHour(0).plusHours(1).toDate())
    }

  }

  object calcReminderDate extends OptionalDateField(this) with LifecycleCallbacks {

    override def beforeSave {
      calcReminderDate.setBox(ReminderHelpers.calculateReminderDate(dueDate.valueBox, reminder.get, reminderAlertAdvance.get))
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