package com.agynamix.invoice.model

import net.liftweb.common._
import net.liftweb.util.Helpers._
import com.agynamix.garten.lib.util.LocalizedEnum
import net.liftweb.mongodb.record.BsonRecord
import org.bson.types.ObjectId
import net.liftweb.mongodb.record.field.ObjectIdField
import net.liftweb.mongodb.record.field.ObjectIdRefField
import com.agynamix.garten.lib.field.BsEnumNameField
import com.agynamix.garten.lib.field.BsObjectIdRefField
import scala.xml.NodeSeq
import scala.xml.Text
import com.agynamix.garten.lib.field.BsTextareaField
import com.agynamix.garten.model.Client
import net.liftweb.mongodb.record.BsonMetaRecord
import net.liftweb.http.SHtml
import com.agynamix.garten.lib.BsonRecordDataSourceTrait
import com.agynamix.garten.lib.field.BsIntField
import com.agynamix.garten.lib.field.BsMoneyField
import com.agynamix.garten.lib.util.MoneyHelpers
import com.agynamix.garten.lib.DataSourceProperty
import com.agynamix.garten.lib.field.BsVatField
import com.agynamix.garten.lib.field.BsOptionalObjectIdRefField
import com.agynamix.garten.lib.field.BsBooleanField
import net.liftweb.mongodb.record.field.BsonRecordListField
import com.agynamix.garten.lib.field.ItemInvoicedMarkerList

object InvoiceItemUnitType extends Enumeration with LocalizedEnum {
  type InvoiceItemUnitType = Value

  val locKeyBase = "InvoiceItemUnit.type"

  val Piece = Value("piece")
  val Hour  = Value("hour")
  val Day   = Value("day")
  val QM    = Value("qm")
  val CCM   = Value("ccm")
  val KWH   = Value("kwh")

}

object InvoiceItem extends InvoiceItem with BsonMetaRecord[InvoiceItem] with Loggable {

  def createInstance(invoice: InvoiceItemContainer[_]) = InvoiceItem.createRecord.clientId(invoice.clientId.get)

  def createInstance(invoice: InvoiceItemContainer[_], invoiceArticle: InvoiceArticle): InvoiceItem = {
    createInstance(invoice).
    articleType(invoiceArticle.articleType.get).
    referencedArticle(invoiceArticle.id.get).
    units(0).
    unitType(invoiceArticle.unitType.get).
    vat(invoiceArticle.vat.get).
    amount(invoiceArticle.amount.get).
    description(invoiceArticle.description.get)
  }

  def createManualArticleCopy(invoice: InvoiceItemContainer[_], tpl: InvoiceItem, position: Int): InvoiceItem = {
    createInstance(invoice).pos(position).articleType(tpl.articleType.get).units(tpl.units.get).unitType(tpl.unitType.get).
               amount(tpl.amount.get).vat(tpl.vat.get).description(tpl.description.get).
               referencedArticle(tpl.referencedArticle.get)
  }

  def createCopyWithArticle(invoice: InvoiceItemContainer[_], tpl: InvoiceItem, article: InvoiceArticle, position: Int): InvoiceItem = {
    val item = createInstance(invoice).pos(position).articleType(article.articleType.get).units(tpl.units.get).unitType(article.unitType.get).
               amount(article.amount.get).vat(article.vat.get).description(article.description.get).
               referencedArticle(article.id.get)
    item
  }

}

class InvoiceItem extends BsonRecord[InvoiceItem] with BsonRecordDataSourceTrait[InvoiceItem] {
  def meta = InvoiceItem

  val datasourceModule = "item"

  def defaultIdValue = ObjectId.get

  object id extends ObjectIdField(this) {
    override def name = "_id"
    override def defaultValue = defaultIdValue
    override def shouldDisplay_? = false
  }

  object clientId extends ObjectIdRefField(this, Client)

  object referencedArticle extends BsOptionalObjectIdRefField(this, InvoiceArticle)

  object pos extends BsIntField(this)

  object articleType extends BsEnumNameField(this, InvoiceArticleType) {
    override def defaultValue = InvoiceArticleType.Manual
  }

  object units extends BsIntField(this)

  object unitType extends BsEnumNameField(this, InvoiceItemUnitType) {
    override def buildDisplayList = InvoiceItemUnitType.buildDisplayList(enum.values.toList)
    override def asHtml = InvoiceItemUnitType.asHtml(get)
  }

  object amount extends BsMoneyField(this)

  object vat extends BsVatField(this) {
    def getClientId = owner.clientId.get
  }

  object description  extends BsTextareaField(this, 5000) {
    override def textareaRows = 3
  }

  object isCompleted extends BsBooleanField(this) {
    override def defaultValue = false
  }

  /**
   * One record per appearance on an invoice is created
   */
  object markInvoiced extends ItemInvoicedMarkerList(this)

  /**
   * Total in cent
   */
  def total = {
    amount.get.multipliedBy(units.get)
  }

  def totalFormatted = MoneyHelpers.moneyToString(total)

  override def clientGetRawProperty = {
    case "totalFormatted" => totalFormatted
  }

}
