package com.agynamix.invoice.model

import net.liftweb.mongodb.record.BsonRecord
import net.liftweb.common._
import net.liftweb.mongodb.record.BsonMetaRecord
import com.agynamix.garten.lib.field.BsObjectIdRefField
import com.agynamix.garten.model.Membership
import com.agynamix.garten.model.Client
import com.agynamix.garten.lib.field.BsStringField
import net.liftweb.mongodb.record.field.BsonRecordListField
import com.agynamix.garten.lib.BsonRecordDataSourceTrait
import com.agynamix.garten.lib.DataSource
import com.agynamix.garten.lib.field.BsIntField
import org.joda.money.Money
import org.joda.money.CurrencyUnit
import com.agynamix.garten.lib.util.MoneyHelpers
import java.math.RoundingMode
import com.agynamix.garten.lib.field.BsMoneyField
import com.agynamix.garten.lib.DataSourceProperty
import com.agynamix.garten.lib.DataSourceProperty
import com.agynamix.garten.model.Garden
import com.agynamix.invoice.lib.InvoiceItemGenerator
import com.agynamix.document.EmptyDataProvider
import com.agynamix.document.SimpleDataProvider
import com.agynamix.garten.model.PropertyValue

object GeneratedInvoice extends GeneratedInvoice with BsonMetaRecord[GeneratedInvoice] with Loggable {

  def createInvoice(invoice: InvoiceContainer, recipient: Membership, garden: Garden, currentInvoice: Box[GeneratedInvoice]): GeneratedInvoice = {
    val rec = GeneratedInvoice.createRecord.recipient(recipient.id.get).garden(garden.id.get)
    val invoiceNo = currentInvoice.map(_.invoiceNo.get) openOr NumberRange.atomicNextValue(invoice.numberRange.get)
    rec.invoiceNo(invoiceNo).clientId(invoice.clientId.get)
    var position = 1

    // for garden
    //   - find all dynamic invoice items (read: the Miracle)
    //   - process them, create static InvoiceItems
    //   - attach static items to GeneratedInvoice

    logger.info(s"Run for recipient ${recipient.displayName} and garden ${garden.garden_no.get}")

    /*
     * Build a DataProvider of sorts. This could mean extending the already existing DataSource/DataProvider that
     * is used for document creation for value extraction. What we would need to do:
     * - needs to return value types other than string
     * - needs to return lists of sub providers found
     */
    val dataProvider = new SimpleDataProvider(garden)

    val gardenItems: List[InvoiceItem] = (for (itemTpl <- garden.invoiceItems.get) yield {
      logger.debug(s"Run for Item TPL ${itemTpl.description.get}")
      val generatedItems = InvoiceItemGenerator.generateInvoiceItems(invoice, recipient, dataProvider, itemTpl, position)
      position = generatedItems.lastOption.map(_.pos.get + 1) getOrElse position
      generatedItems
    }).flatten

    // Now add the positions directly stored in the invoice container
    val recipientDataProvider = new SimpleDataProvider(recipient)
    val items = (for (itemTpl <- invoice.invoiceItems.get) yield {
      val generatedItems = InvoiceItemGenerator.generateInvoiceItems(invoice, recipient, recipientDataProvider, itemTpl, position)
      position = generatedItems.lastOption.map(_.pos.get + 1) getOrElse position
      generatedItems
    }).flatten
    rec.invoiceItems(gardenItems ::: items)
    rec.netTotal(rec.invoiceItems.get.foldLeft(Money.zero(rec.netTotal.defaultCurrency))((accu, item) => accu.plus(item.total)))
    rec.grossTotal(collectTaxItems(rec.invoiceItems.get).foldLeft(Money.zero(rec.grossTotal.defaultCurrency))((accu, item) => accu.plus(item.grossAmount)))
    rec
  }


}

case class TaxItem(vat: Vat, netAmount: Money, vatAmount: Money, grossAmount: Money) extends DataSource {
  val datasourceModule = "taxItem"

  def getRawProperty(key: String): Box[Any] = key match {
    case "vat"         => Full(vat.vat.asHtml.toString())
    case "netAmount"   => Full(MoneyHelpers.moneyToString(netAmount))
    case "vatAmount"   => Full(MoneyHelpers.moneyToString(vatAmount))
    case "grossAmount" => Full(MoneyHelpers.moneyToString(grossAmount))
    case _             => Empty
  }

}

class GeneratedInvoice extends BsonRecord[GeneratedInvoice] with BsonRecordDataSourceTrait[GeneratedInvoice] {
  def meta = GeneratedInvoice

  val datasourceModule = "doc"

  override def getRepeatedElems(key: String): List[DataSource] = {
    key match {
      case "invoiceItems" => invoiceItems.get
      case "taxItems" => (for (client <- clientId.obj) yield {
        if (client.deductVat.get) collectTaxItems else Nil
      }) openOr collectTaxItems
      case _          => Nil
    }
  }

  def collectTaxItems: List[TaxItem] = collectTaxItems(invoiceItems.get)

  def collectTaxItems(invItems: List[InvoiceItem]): List[TaxItem] = {
    val grouped = invItems.groupBy(_.vat.get).toList
    val items = grouped.flatMap{ case (vatId, itemList) =>
      for (vat <- Vat.find(vatId)) yield {
        val netAmount   = itemList.foldLeft(Money.zero(CurrencyUnit.EUR))((accu, item) => accu.plus(item.total))
        val vatAmount   = netAmount.multipliedBy(vat.vat.percentToDouble, RoundingMode.HALF_UP)
        val grossAmount = netAmount.plus(vatAmount)
        TaxItem(vat, netAmount, vatAmount, grossAmount)
      }
    }
    items
  }

  object clientId extends BsObjectIdRefField(this, Client)

  object invoiceNo extends BsStringField(this, 50) {
    override def displayName = "Rechnungsnr."
  }

  object recipient extends BsObjectIdRefField(this, Membership)

  object garden extends BsObjectIdRefField(this, Garden)

  object netTotal extends BsMoneyField(this)

  object grossTotal extends BsMoneyField(this)

  object invoiceItems extends BsonRecordListField(this, InvoiceItem)

}

