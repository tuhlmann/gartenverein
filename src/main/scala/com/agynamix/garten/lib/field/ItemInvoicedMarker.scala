package com.agynamix.garten.lib.field

import net.liftweb.mongodb.record.BsonRecord
import net.liftweb.mongodb.record.field.DateField
import net.liftweb.mongodb.record.BsonMetaRecord
import net.liftweb.common._
import net.liftweb.common.Box._
import net.liftweb.util.Helpers._
import net.liftweb.mongodb.record.field.BsonRecordListField
import java.util.Date
import com.agynamix.invoice.model.InvoiceContainer
import org.bson.types.ObjectId

class ItemInvoicedMarker extends BsonRecord[ItemInvoicedMarker] {
  def meta = ItemInvoicedMarker

  /**
   * The date this item has been invoiced
   */
  object date         extends DateField(this) with DateFormAdapterField with GermanDateField {
    override def displayName = "Datum"
    override def defaultValue = new Date()
  }

  /**
   * The id of the invoice in which this item appears
   */
  object invoiceId    extends BsOptionalObjectIdRefField(this, InvoiceContainer)

  object invoiceYear  extends OptionalBsIntField(this)

  object generatedInvoiceNo extends BsStringField(this, 50)

  object isCompleted  extends BsBooleanField(this) {
    override def defaultValue = false
  }

}

object ItemInvoicedMarker extends ItemInvoicedMarker with BsonMetaRecord[ItemInvoicedMarker] with Loggable {

  def createInstance(invoiceId: ObjectId, generatedInvoiceNo: String, invoiceYear: Box[Int], invoiceDate: Date) =
    ItemInvoicedMarker.createRecord
                      .invoiceId(invoiceId)
                      .generatedInvoiceNo(generatedInvoiceNo)
                      .invoiceYear(invoiceYear)
                      .date(invoiceDate)

}

abstract class ItemInvoicedMarkerList[OwnerType <: BsonRecord[OwnerType]](rec: OwnerType)
         extends BsonRecordListField(rec, ItemInvoicedMarker) {

  def addMarker(invoiceId: ObjectId, generatedInvoiceNo: String, invoiceYear: Box[Int], invoiceDate: Date): ItemInvoicedMarker = {
    val value = ItemInvoicedMarker.createInstance(invoiceId, generatedInvoiceNo, invoiceYear, invoiceDate).isCompleted(true)
    atomicUpdate(lst => value :: lst)
    value
  }

  /**
   * Check if a record exists for the given year
   */
  def notMarkedForYear(year: Int): Boolean = {
    get.exists(marker => {
      marker.invoiceYear.get === year && marker.isCompleted.get
    })
  }

}