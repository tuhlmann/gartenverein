package com.agynamix.garten.model

import net.liftweb.common._
import net.liftweb.mongodb.record.field.ObjectIdRefField
import net.liftweb.record.field.EnumNameField
import java.util.Date
import com.foursquare.rogue.LiftRogue._
import net.liftweb.record.field._
import net.liftweb.mongodb.{ JsonObjectMeta, JsonObject }
import net.liftweb.mongodb.record._
import net.liftweb.mongodb.record.field._
import net.liftweb.common.{ Box, Full, Empty }
import org.bson.types.ObjectId
import net.liftweb.util.Helpers._
import org.joda.time.{ DateTime, DateTimeConstants }
import net.liftweb.util.Props
import com.mongodb.WriteConcern
import java.util.Calendar
import org.joda.time.LocalDate
import com.agynamix.garten.model.share.CreatedUpdated

object ScheduledJobType extends Enumeration {
  type ScheduledJobType = Value
  val Once      = Value("once")
  val Recurring = Value("recurring")
}

object ScheduledJobState extends Enumeration {
  type ScheduledJobState = Value
  val Open       = Value("open")
  val InProgress = Value("in_progress")
  val Closed     = Value("closed")
}

object ScheduledJobActionType extends Enumeration {
  type ScheduledTaskActionType = Value
  val SomeJobTemplateForLater = Value("some_job")
}

/**
 * One ScheduledTask record is a task that should be run in the future. The record describes the task to run
 * and the necessary parameters.
 */
object ScheduledJob extends ScheduledJob with MongoMetaRecord[ScheduledJob] {

  /**
   * Ensure that static jobs exist, create them if necessary.
   * This is started from boot and runs at initialization time
   */
  def init(): Unit = {
    resetInProgressJobs
//    (ScheduledJob where (_.jobActionType eqs ScheduledJobActionType.SomeJobTemplateForLater) get).getOrElse{
//      createWeeklyAnalyticsJob
//    }
  }

//  def createSomeJob() = {
//    createRecurringJob.jobActionType(ScheduledJobActionType.SomeJobTemplateForLater).
//    due(scheduleNextWeeklyEmailReportRun.toDate()).save
//  }

  def createRecurringJob() = createJob.jobType(ScheduledJobType.Recurring)
  def createJob() = ScheduledJob.createRecord.jobType(ScheduledJobType.Once).jobState(ScheduledJobState.Open)

  def scheduleNextWeeklyEmailReportRun(): DateTime = {
    Props.mode match {
      case Props.RunModes.Development => 1.day.later
      case _ =>
        val d = new DateTime().toDateMidnight().plusWeeks(1).withDayOfWeek(DateTimeConstants.MONDAY).toDateTime()
        logger.info("Reschedule weekly report for: "+d)
        d
    }
  }

  def findNextJob: Box[ScheduledJob] = {
    val timeNow = new DateTime()

    ScheduledJob where (_.due before timeNow) and (_.jobState eqs ScheduledJobState.Open) findAndModify
      (_.jobState setTo ScheduledJobState.InProgress) and (_.inProgressStart setTo timeNow) updateOne (true)
  }

  def resetInProgressJobs(): Unit = tryo {
    (ScheduledJob where (_.jobState eqs ScheduledJobState.InProgress) fetch ()).foreach(_.jobState(ScheduledJobState.Open).save(true))
  }

}

class ScheduledJob private () extends MongoRecord[ScheduledJob] with ObjectIdPk[ScheduledJob] with CreatedUpdated[ScheduledJob] with Loggable {

  import ScheduledJob._

  def meta = ScheduledJob

  object jobType       extends EnumNameField(this, ScheduledJobType)
  object jobState      extends EnumNameField(this, ScheduledJobState)
  object jobActionType extends EnumNameField(this, ScheduledJobActionType)

  object due extends DateField(this)

  // set the time when work at an action started, in case it aborts and needs to be reopened
  object inProgressStart extends DateField(this)

  // marks when this task has run the last time (only if recurring task)
  object lastRun extends DateField(this)

  def close = {
    jobState(ScheduledJobState.Closed).save(true)
  }

  def finish: ScheduledJob = {
    lastRun(new Date())
  }

  def rescheduleOrRemove: Boolean = {
    if (jobType.get == ScheduledJobType.Once) {
      this.delete_!
    } else {
      this.finish.jobState(ScheduledJobState.Open)
      jobActionType.get match {
        case ScheduledJobActionType.SomeJobTemplateForLater => due(scheduleNextWeeklyEmailReportRun.toDate())
        case _ => this.jobState(ScheduledJobState.Closed) /* do nothing */
      }
      this.save(true)
      true
    }
  }

}