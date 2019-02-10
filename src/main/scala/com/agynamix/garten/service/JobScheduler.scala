package com.agynamix.garten.service

import net.liftweb.actor.LiftActor
import net.liftweb.http.ListenerManager
import net.liftweb.common.Loggable
import net.liftweb.common.Empty
import net.liftweb.actor.LAPinger
import net.liftweb.util.Helpers._
import java.util.Date
import net.liftweb.common.Full
import scala.xml.Text
import net.liftweb.util.Helpers._
import net.liftweb.util.CssSel
import net.liftweb.http.Templates
import net.liftweb.json.JsonDSL._
import org.joda.time.DateTime
import org.joda.time.LocalDate
import org.joda.time.DateTimeConstants
import net.liftweb.util.Props
import com.agynamix.garten.model._
import com.agynamix.garten.lib.MailSender

case object FindScheduledJobs
case object FinishedJobProcessing

object JobScheduler extends LiftActor with Loggable {

  lazy val firstRun = if (Props.devMode) 1 minute else 5 minutes
  lazy val nextRun  = if (Props.devMode) 1 minute else 5 minutes

  def init(): Unit = {
    logger.info("Initialize TaskScheduler")
    ScheduledJob.init
    Task.init
    Event.init
    reschedule(firstRun)
  }

  def reschedule(delay: Long = nextRun) = LAPinger.schedule(this, FindScheduledJobs, delay)

  def messageHandler = {
    case FindScheduledJobs =>
      try {
        logger.debug("Searching scheduled jobs")
        val inProgressStart = new Date()
        var isDone = false
        var processed = 0
        do {
          Task.findNextDueItem.map(t => {TaskProcessor ! t; processed += 1}) openOr (isDone = true)
        } while (!isDone)
        isDone = false
        do {
          Event.findNextDueItem.map(t => {TaskProcessor ! t; processed += 1}) openOr (isDone = true)
        } while (!isDone)
        isDone = false
        do {
          ScheduledJob.findNextJob.map(t => {TaskProcessor ! t; processed += 1}) openOr (isDone = true)
        } while (!isDone)
        StatusLog.logSchedulerJob(StatusLogReturnCode.Ok, processed)
        if (processed > 0) {
          logger.info("Executed JobScheduler with RC 0, processed records: "+processed)
        }
      } catch {
          case e: Throwable =>
            logger.error("Error running JobScheduler", e)
            StatusLog.logSchedulerJob(StatusLogReturnCode.ErrorWithException, 0)
      }
      reschedule()

    case msg =>
      logger.warn("Unidentified message of type "+msg.getClass.getName)
      StatusLog.logSchedulerJob(StatusLogReturnCode.Error, 0)
      reschedule()

  }
}

object TaskProcessor extends LiftActor with Loggable {

  def runSafe(jobId: String)(block: => Unit) = {
    try {
      block
    } catch {
      case e: Exception => logger.error("Error executing Job %s: %s".format(jobId, e.getMessage()))
    }
  }

  def messageHandler = {

    case dueTask: Task =>
      runSafe(dueTask.id.get.toString) {
        logger.info("Process this Note: "+dueTask.subject.get)
        MailSender.sendTaskReminder(dueTask)
      }
      dueTask.rescheduleOrDone

    case dueEvent: Event =>
      runSafe(dueEvent.id.get.toString) {
        logger.info("Process this Event: "+dueEvent.subject.get)
        MailSender.sendEventReminder(dueEvent)
      }
      dueEvent.rescheduleOrDone


    case someJobTemplate: ScheduledJob if someJobTemplate.jobActionType.get == ScheduledJobActionType.SomeJobTemplateForLater =>
      runSafe(someJobTemplate.id.get.toString()) {
        logger.info("Sending weekly analytics reports")
//        println("XXX Sending weekly analytics reports")
//        Calc start and end: Monday thru Sunday
        val dateNow = new DateTime(someJobTemplate.due.get.getTime())
        val reportEnd = new LocalDate(dateNow).toDateTimeAtStartOfDay().withDayOfWeek(DateTimeConstants.MONDAY)
        val reportStart = reportEnd.minusWeeks(1)
        //MailSender.sendWeeklyAnalyticsReport(reportStart.toDateTime(), reportEnd.toDateTime())
      }
      someJobTemplate.rescheduleOrRemove

    case msg =>
      logger.warn("Unidentified message of type "+msg.getClass.getName)

  }

}
