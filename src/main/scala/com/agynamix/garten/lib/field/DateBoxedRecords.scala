package com.agynamix.garten.lib.field

import net.liftweb.mongodb.record.BsonRecord
import net.liftweb.mongodb.record.BsonMetaRecord
import net.liftweb.mongodb.record.field.DateField
import java.util.Date
import com.agynamix.invoice.model.InvoiceContainer
import net.liftweb.mongodb.record.field.BsonRecordListField
import net.liftweb.util.Helpers._
import scala.xml.NodeSeq
import net.liftweb.common._
import net.liftweb.http.js.JsCmds._
import scala.xml.Elem
import com.agynamix.document.MarkInvoicedData

class DateBoxedLongValue extends BsonRecord[DateBoxedLongValue] {
  def meta = DateBoxedLongValue

  object date         extends DateField(this) with DateFormAdapterField with GermanDateField {
    override def displayName = "Datum"
    override def defaultValue = new Date()
  }

  object value        extends OptionalBsLongField(this) {
    override def displayName = "Wert"
  }

  object invoiceId    extends BsOptionalObjectIdRefField(this, InvoiceContainer)

  object generatedInvoiceNo extends BsStringField(this, 50)

  object isCompleted  extends BsBooleanField(this) {
    override def defaultValue = false
  }

}

object DateBoxedLongValue extends DateBoxedLongValue with BsonMetaRecord[DateBoxedLongValue] with Loggable {

  def createInstance() = DateBoxedLongValue.createRecord

  def emptyHeadOrCreate(list: BsonRecordListField[_, DateBoxedLongValue]): DateBoxedLongValue = {
    val head = list.get headOr addNewValue(list)
    if (head.value.get.isEmpty) head else addNewValue(list)
  }

  def firstNonEmptyOpt(list: BsonRecordListField[_, DateBoxedLongValue]): Option[DateBoxedLongValue] = {
    list.get.find(_.value.get.isDefined)
  }

  private def addNewValue(list: BsonRecordListField[_, DateBoxedLongValue]): DateBoxedLongValue = {
    val value = DateBoxedLongValue.createInstance
    list.atomicUpdate(lst => value :: lst)
    value
  }


}

abstract class DateBoxedLongValueList[OwnerType <: BsonRecord[OwnerType]](rec: OwnerType)
         extends BsonRecordListField(rec, DateBoxedLongValue) {

  lazy val emptyHeadValue   = DateBoxedLongValue.emptyHeadOrCreate(this)
  lazy val firstNonEmptyOpt = DateBoxedLongValue.firstNonEmptyOpt(this)
  lazy val hiddenRowsId     = randomString(12)

//  override def toForm = {
//    val first = headOrCreate
//  }

  private def elem: NodeSeq = {

    <table class="table table-condensed table-responsive table-no-borders table-tiny-spacing">
      <thead>{renderValue(emptyHeadValue, true, true, get.tail.nonEmpty)}</thead>
      <tbody id={hiddenRowsId} style="display:none;">
        {renderExistingValues}
      </tbody>
    </table>
  }

  private def renderValue(rec: DateBoxedLongValue, renderAddBtn: Boolean, renderExpand: Boolean, expandDisabled: Boolean): NodeSeq = {
    <tr>
      {
        if (!rec.isCompleted.get) {
          <td>{rec.date.toForm.openOr(NodeSeq.Empty)}</td>
          <td>{rec.value.toForm.openOr(NodeSeq.Empty)}</td>
        } else {
          <td><span class="form-control-static indented-span">{rec.date.asHtml}</span></td>
          <td><span class="form-control-static indented-span">{rec.value.asHtml}</span></td>
        }
      }
      <td>{
        if (renderExpand) {
          <button type="button" class={"btn btn-info " + (if (!expandDisabled) "disabled" else "")} data-toggle="tooltip"
                  title="Gespeicherte Werte" onclick={"$('#%s').slideToggle()".format(hiddenRowsId)}>
            <i class="icon-ellipsis-horizontal"></i>
          </button>
        } else NodeSeq.Empty
      }</td>
    </tr>
  }

  def renderExistingValues = {
    get.tail.map(rec => renderValue(rec, false, false, false))
  }

  override def toForm: Box[NodeSeq] = Full(elem)

  override def asHtml = {
    firstNonEmptyOpt.map(v => {
      <span>{v.date.asText}: {v.value.asHtml}</span>
    }).getOrElse(NodeSeq.Empty)
  }

  /**
   * Go backward, find the first invoiced value, subtract this from the current one
   */
  def deltaValueNotInvoiced: Long = {
    if (get.headOption.exists(_.isCompleted.get)) {
      // If last entry is invoiced we return 0
      0l
    } else {
      val firstInvoicedValue = get.find(_.isCompleted.get).flatMap(_.value.get) getOrElse 0l
      val firstNotInvoiced = get.find(v => (!v.isCompleted.get && v.value.get.isDefined)).flatMap(_.value.get) getOrElse 0l
      (firstNotInvoiced - firstInvoicedValue) max 0l
    }
  }

  def markValueInvoiced(markInvoicedData: MarkInvoicedData): Unit = {
    println("Mark as Invoiced: "+this.name)
    var firstCompletedFound = false
    get.foreach(v => {
      println("Check value "+v.value.get+", isCompleted: "+v.isCompleted.get)
      if (v.isCompleted.get) firstCompletedFound = true
      if (!firstCompletedFound) {
        println("Set completed: "+v.value.get)
        v.isCompleted(true)
         .date(markInvoicedData.invoiceDate)
         .invoiceId(markInvoicedData.invoiceId)
         .generatedInvoiceNo(markInvoicedData.generatedInvoiceNo)
      }
    })
  }

}