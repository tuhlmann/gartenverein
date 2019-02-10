package com.agynamix.garten.lib.field

import com.agynamix.garten.lib.util.LocalizedEnum
import net.liftweb.mongodb.record.BsonRecord
import net.liftweb.common.{Empty, Full, Box}
import scala.xml.{Elem, NodeSeq}
import net.liftweb.util.Helpers._
import net.liftweb.common.Full
import net.liftweb.http.{SHtml, S}
import net.liftweb.http.js.JsCmds.{Run, OnLoad, Script, Noop}
import net.liftweb.http.S.{SFuncHolder, AFuncHolder}
import net.liftweb.http.SHtml.SelectableOption
import net.liftweb.http.js.jquery.JqJsCmds
import org.bson.types.ObjectId
import java.util.Date
import org.joda.time.DateTime

object ReminderType extends Enumeration with LocalizedEnum {
  type ReminderType = Value

  val locKeyBase = "note.reminder"

  val None       = Value("none")
  val SameDay    = Value("sameDay")
  val DayBefore  = Value("dayBefore")
  val WeekBefore = Value("weekBefore")

  def displayValue(t: ReminderType.Value) = localized(t)

}

object ReminderAlertAdvanceType extends Enumeration with LocalizedEnum {
  type ReminderAlertAdvanceType = Value

  val locKeyBase = "note.reminder.alert_advance"

  val AtStart    = Value("start")
  val Minutes_5  = Value("min_5")
  val Minutes_10 = Value("min_10")
  val Minutes_30 = Value("min_30")
  val Hour_1     = Value("hour_1")

  def displayValue(t: ReminderAlertAdvanceType.Value) = localized(t)

  def minusMinutes(t: ReminderAlertAdvanceType.Value): Int = {
    t match {
      case ReminderAlertAdvanceType.Minutes_5  => 5
      case ReminderAlertAdvanceType.Minutes_10 => 10
      case ReminderAlertAdvanceType.Minutes_30 => 30
      case ReminderAlertAdvanceType.Hour_1     => 60
      case _                                   => 0
    }
  }

}

trait ReminderHelpers {

  def calculateReminderDate(dueDate: Box[Date], reminderType: ReminderType.Value, alertAdvance: ReminderAlertAdvanceType.Value): Box[Date] = {
    dueDate.flatMap(d => {
      val date = new DateTime(d)
      reminderType match {
        case ReminderType.SameDay    => Full(date.minusMinutes(ReminderAlertAdvanceType.minusMinutes(alertAdvance)).toDate)
        case ReminderType.DayBefore  => Full(date.minusDays(1).toDate())
        case ReminderType.WeekBefore => Full(date.minusWeeks(1).toDate())
        case _ => Empty
      }
    })
  }

}

object ReminderHelpers extends ReminderHelpers

abstract class ReminderTypeField[OwnerType <: BsonRecord[OwnerType]](rec: OwnerType, alertAdvance: ReminderAlertAdvanceField[_])
         extends BsEnumNameField(rec, ReminderType) {

  override def displayName = "Erinnerung"
  override def defaultValue = ReminderType.None

  override def buildDisplayList: List[(Box[ReminderType.Value], String)] = {
    val options = enum.values.toList.map(a => (Full(a), ReminderType.displayValue(a)))
    if (optional_?) (Empty, emptyOptionLabel)::options else options
  }

  private def elem: NodeSeq = {
    val reminderTypeId: String = uniqueFieldId.map(id => s"${id}_remtype").openOr(randomString(12))
    val reminderAdvanceSpanId = nextFuncName
    val reminderType: NodeSeq = {
      val opt = buildDisplayList.flatMap(r => r._1.map((_, r._2)))

      SHtml.ajaxSelectObj[ReminderType.Value](opt, valueBox,
                     (s: ReminderType.Value) => {
                       set(s)
                       if (s == ReminderType.SameDay) {
                         JqJsCmds.Show(reminderAdvanceSpanId)
                       } else {
                         JqJsCmds.Hide(reminderAdvanceSpanId)
                       }
                     },
                     ("tabindex" -> tabIndex.toString),
                     ("id" -> reminderTypeId),
                     ("class" -> "form-control input-large"),
                     ("style" -> "display:inline-block"))
    }

    <div>
      <span>{reminderType}</span>&nbsp;
      <span id={reminderAdvanceSpanId} style={if (valueBox === ReminderType.SameDay) "" else "display:none;"}>
        {alertAdvance.toForm.openOr(NodeSeq.Empty)}
      </span>
    </div>
  }

  override def toForm: Box[NodeSeq] = Full(elem)


}

abstract class ReminderAlertAdvanceField[OwnerType <: BsonRecord[OwnerType]](rec: OwnerType) extends BsEnumNameField(rec, ReminderAlertAdvanceType) {
  override def displayName = "Erinnerungszeitpunkt"
  override def defaultValue = ReminderAlertAdvanceType.AtStart

  override def buildDisplayList: List[(Box[ReminderAlertAdvanceType.Value], String)] = {
    val options = enum.values.toList.map(a => (Full(a), ReminderAlertAdvanceType.displayValue(a)))
    if (optional_?) (Empty, emptyOptionLabel)::options else options
  }

  private def elem: NodeSeq = {
    val reminderAdvanceId: String = uniqueFieldId.map(id => s"${id}_remadvance").openOr(randomString(12))
    SHtml.selectObj[Box[ReminderAlertAdvanceType.Value]](buildDisplayList, Full(valueBox), setBox(_)) %
                   ("tabindex" -> tabIndex.toString) %
                   ("id" -> reminderAdvanceId) %
                   ("class" -> "form-control input-large") %
                   ("style" -> "display:inline-block")
  }

  override def toForm: Box[NodeSeq] = Full(elem)


}
