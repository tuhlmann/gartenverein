package com.agynamix.garten.lib.field

import net.liftweb.mongodb.record.BsonRecord
import net.liftweb.mongodb.record.field.OptionalDateField
import net.liftweb.record.LifecycleCallbacks
import java.text.SimpleDateFormat
import net.liftweb.mongodb.record.field.DateTypedField
import com.agynamix.garten.model.share.UniqueFieldId
import net.liftweb.util.Helpers._
import net.liftweb.common._
import java.util.Date
import net.liftweb.http.S
import net.liftweb.http.js.JsCmds._
import scala.xml.NodeSeq
import scala.xml.Text
import org.joda.time.DateTime
import org.joda.time.format.DateTimeParser
import net.liftweb.mongodb.record.field.DateField
import net.liftweb.record.Record
import com.agynamix.garten.model.User
import org.joda.time.format.DateTimeFormat

trait GermanDateField {
  val fieldDateMask      = "dd.MM.yyyy"
  lazy val fieldDateFormatter = {
    val fmt = new SimpleDateFormat(fieldDateMask)
    fmt.setTimeZone(User.getTimeZone)
    fmt
  }
  val fieldTimeMask      = "HH:mm"
  lazy val fieldTimeFormatter = {
    val fmt = new SimpleDateFormat(fieldTimeMask)
    fmt.setTimeZone(User.getTimeZone)
    fmt
  }
  def fieldDateFormatter(user: User) = {
    val fmt = new SimpleDateFormat(fieldDateMask)
    fmt.setTimeZone(user.timezone.isAsTimeZone)
    fmt
  }
  def fieldTimeFormatter(user: User) = {
    val fmt = new SimpleDateFormat(fieldTimeMask)
    fmt.setTimeZone(user.timezone.isAsTimeZone)
    fmt
  }
  val fieldInputMask = "99.99.9999"
  //val fmt = DateTimeFormat.forPattern("MMMM, yyyy").withZone(User.getDateTimeZone)
}

trait DateFormAdapterField extends DateTypedField with UniqueFieldId {

  OwnerType =>

  def fieldDateFormatter: SimpleDateFormat
  def fieldInputMask: String

  private def formatDate(d: Date): String = fieldDateFormatter.format(d)

  override def setFromString(s: String): Box[Date] = s match {
    case null|"" if optional_? => setBox(Empty)
    case null|"" => setBox(Failure(notOptionalErrorMessage))
    case other => setBox(tryo(fieldDateFormatter.parse(s)))
  }

  def elem = {
    val fieldId: String = uniqueFieldId.openOr(randomString(12))

    S.fmapFunc(S.SFuncHolder(this.setFromString(_))) { funcName =>
      <span id={fieldId} class="input-group date date-input input-medium" style="display:inline-table;">
        <input name={funcName} type="text" class="value form-control" value={valueBox.map(formatDate) openOr ""}
               tabindex={ tabIndex.toString }/>
        <span class="input-group-addon add-on remove-if-readonly"><i class="icon-calendar"></i></span>
      </span> ++ <div class="remove-if-readonly">{Script(OnLoad(Run("""
          |$('#%s').datepicker({
          |  language: 'de',
          |  autoclose: true
          |});""".stripMargin.format(fieldId))))}</div>
    }
  }

  override def toForm: Box[NodeSeq] = Full(elem)

  override def asHtml = valueBox.map( d => Text(fieldDateFormatter.format(d.getTime()))) getOrElse Text("")

  def asText = valueBox.map( d => fieldDateFormatter.format(d.getTime())) getOrElse ""

}

trait DateTimeFormAdapterField extends DateTypedField with UniqueFieldId {

  OwnerType =>

  def fieldDateFormatter: SimpleDateFormat
  def fieldTimeFormatter: SimpleDateFormat
  def fieldDateFormatter(user: User): SimpleDateFormat
  def fieldTimeFormatter(user: User): SimpleDateFormat
  def fieldDateMask: String
  def fieldTimeMask: String
  def fieldInputMask: String

  private def formatDate(d: Date): String = fieldDateFormatter.format(d)
  private def formatTime(d: Date): String = fieldTimeFormatter.format(d)

  private def setDatePart(s: String): Unit = {
    if (s.trim().nonEmpty) {
      try {
        val d = new DateTime(this.valueBox.openOr(new Date()), User.getDateTimeZone)
        val datePart = new DateTime(fieldDateFormatter.parse(s), User.getDateTimeZone)
        val re = d.withDate(datePart.year().get(), datePart.monthOfYear().get(), datePart.dayOfMonth().get())
        this.setBox(Full(re.toDate))
      } catch {
        case e: Exception => e.printStackTrace();
      }
    }
  }

  private def setTimePart(s: String): Unit = {
    if (s.trim().nonEmpty) {
      try {
        val d = new DateTime(this.valueBox.openOr(new Date()), User.getDateTimeZone)
        val timePart = new DateTime(fieldTimeFormatter.parse(s), User.getDateTimeZone)
        val re = d.withTime(timePart.hourOfDay().get(), timePart.minuteOfHour().get(), 0, 0)
        this.setBox(Full(re.toDate))
      } catch {
        case e: Exception => e.printStackTrace();
      }
    }
  }

  override def setFromString(s: String): Box[Date] = s match {
    case null|"" if optional_? => setBox(Empty)
    case null|"" => setBox(Failure(notOptionalErrorMessage))
    case other => setBox(tryo(fieldDateFormatter.parse(s)))
  }

  private def elem: NodeSeq = {
    val dateId: String = uniqueFieldId.map(id => s"${id}_date").openOr(randomString(12))
    val date: NodeSeq = {

      S.fmapFunc(S.SFuncHolder(this.setDatePart(_))) { funcName =>
        <span id={dateId} class="input-group date date-input input-medium" style="display:inline-table;">
          <input name={funcName} type="text" class="value form-control"
                 value={valueBox.map(formatDate) openOr ""}
                 tabindex={ tabIndex.toString }/>
          <span class="input-group-addon add-on remove-if-readonly"><i class="icon-calendar"></i></span>
        </span>
      }
    }

    val timeId: String = uniqueFieldId.map(id => s"${id}_time").openOr(randomString(12))
    val time: NodeSeq = {

      S.fmapFunc(S.SFuncHolder(this.setTimePart(_))) { funcName =>
        <span id={timeId} class="input-group bootstrap-timepicker time-input input-medium" style="display:inline-table;">
          <input name={funcName} type="text" class="value form-control"
                 value={valueBox.map(formatTime) openOr ""}
                 tabindex={ tabIndex.toString }/>
          <span class="input-group-addon add-on remove-if-readonly"><i class="icon-time"></i></span>
        </span>
      }
    }

    <div>
      {date} {time}
      <div class="remove-if-readonly">{Script(OnLoad(Run("""
        |$('#%s').datepicker({
        |  language: 'de',
        |  autoclose: true
        |});""".stripMargin.format(dateId)) &
        Run("""
        |$('#%s input').timepicker({
        |  minuteStep: 15,
        |  showMeridian: false
        |});""".stripMargin.format(timeId))))}
      </div>
    </div>
  }

  override def toForm: Box[NodeSeq] = Full(elem)

  override def asHtml = valueBox.map( d => {
    val dt = new DateTime(d, User.getDateTimeZone)
    if (dt.getMinuteOfDay == 0) {
      Text(fieldDateFormatter.format(d))
    } else {
      Text(fieldDateFormatter.format(d)+" "+fieldTimeFormatter.format(d))
    }
  }) getOrElse Text("")

  def asHtml(user: User) = valueBox.map( d => {
    val dt = new DateTime(d, user.timezone.isAsJodaTimeZone)
    if (dt.getMinuteOfDay == 0) {
      Text(fieldDateFormatter(user).format(d))
    } else {
      Text(fieldDateFormatter(user).format(d)+" "+fieldTimeFormatter(user).format(d))
    }
  }) getOrElse Text("")

}

trait DateTimeFieldHelpers extends DateTypedField {

  /**
   * Using Option instead of Box to get the JValue transformed properly
   */
  def toUnixSeconds: Option[Long] = valueBox.map(v => (v.getTime() / 1000))

}

abstract class BsOptionalDateTimeField[OwnerType <: BsonRecord[OwnerType]](rec: OwnerType)
         extends OptionalDateField(rec) with DateTimeFieldHelpers with DateTimeFormAdapterField with GermanDateField with LifecycleCallbacks {

  override def beforeSave {
    //println("BeforeSave: "+valueBox)
  }
}

abstract class BsDateTimeField[OwnerType <: BsonRecord[OwnerType]](rec: OwnerType)
         extends DateField(rec) with DateTimeFieldHelpers with DateTimeFormAdapterField with GermanDateField with LifecycleCallbacks {

  override def beforeSave {
    //println("BeforeSave: "+valueBox)
  }

}


abstract class BsOptionalDateField[OwnerType <: BsonRecord[OwnerType]](rec: OwnerType)
               extends OptionalDateField(rec) with DateFormAdapterField with GermanDateField {

}

abstract class BsDateField[OwnerType <: BsonRecord[OwnerType]](rec: OwnerType)
               extends DateField(rec) with DateFormAdapterField with GermanDateField {

}

