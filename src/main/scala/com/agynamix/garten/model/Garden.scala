package com.agynamix.garten.model

import net.liftweb.common._
import net.liftweb.util.Helpers._
import net.liftweb.util.FieldContainer
import com.agynamix.garten.model.share._
import com.agynamix.garten.lib.StructuredDbObject
import net.liftweb.json.JsonAST.JObject
import scala.xml.Text
import com.agynamix.garten.lib.util.DateHelpers
import com.agynamix.garten.lib.field.GermanDateField
import net.liftweb.util.FieldError
import net.liftweb.mongodb.record.MongoMetaRecord
import net.liftweb.record.field.IntField
import net.liftweb.record.field.StringField
import net.liftweb.mongodb.BsonDSL._
import com.agynamix.garten.lib.field.BsIntField
import com.agynamix.garten.lib.field.BsStringField
import com.agynamix.garten.lib.field.BsObjectIdRefField
import org.bson.types.ObjectId
import scala.xml.NodeSeq
import net.liftweb.http.SHtml
import net.liftweb.util.BaseField
import com.agynamix.garten.lib.field.BsObjectIdRefListField
import net.liftweb.mongodb.record.field.BsonRecordListField
import net.liftweb.http.S
import com.agynamix.invoice.model.InvoiceItem
import com.agynamix.invoice.model.InvoiceArticleType
import com.agynamix.invoice.lib.InvoiceItemProcessor
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js.jquery.JqJE._
import com.agynamix.invoice.lib.ManualArticleInvoiceItemProcessor
import com.agynamix.invoice.lib.EmptyInvoiceItemProcessor
import com.agynamix.garten.snippet.InvoicesModalDialog
import com.agynamix.invoice.model.InvoiceItemContainer
import com.agynamix.garten.snippet.GardenModalDialog
import com.agynamix.garten.lib.MongoDataSourceTrait
import com.foursquare.rogue.LiftRogue._
import com.agynamix.garten.lib.DataSource
import com.agynamix.garten.lib.field.DateBoxedLongValueList
import org.joda.money.Money
import org.joda.money.CurrencyUnit
import com.agynamix.invoice.model.InvoiceContainer
import com.agynamix.invoice.model.InvoiceFulfillmentType
import com.agynamix.invoice.model.GeneratedInvoice
import com.agynamix.document.SimpleDataProvider
import com.agynamix.invoice.lib.InvoiceItemGenerator
import com.agynamix.document.MarkInvoicedData
import net.liftweb.json.JsonAST.JObject
import net.liftweb.common.Full
import com.agynamix.document.MarkInvoicedData


object Garden extends Garden with LogbookMetaRecord[Garden] with StructuredDbObject[Garden] with FormValidators[Garden] {

  def keyFields(user: User, args: Any*): Box[()=>JObject] =
    user.activeMembership.clientId.map(id => ()=>(clientId.name -> id))

  def createInstance(user: User, client: Client): Garden = Garden.createRecord.clientId(client.id.get)

  def findMembersOfGarden(garden: Garden): List[Membership] = {
    Membership where (_.gardenRef contains garden.id.get) fetch()
  }

//  def findGardens: List[Garden] = (for (user <- User.currentUser) yield {
//    findAll(keyFields(user))
//  }) openOr Nil


  /**
   * Runs through the invoice items attached to this Garden and marks them:
   * - if of type invoice: mark it with invoice id and set as completed
   */
  def markValuesInvoiced(invoice: InvoiceContainer, generatedInvoice: GeneratedInvoice, garden: Garden): Unit = {
    println("Garden.markValuesInvoiced")
    val dataProvider = new SimpleDataProvider(garden)
    garden.invoiceItems.get.foreach(invoiceItem => {
      println("Process invoice item: "+invoiceItem.description.get)
      invoiceItem.referencedArticle.obj match {
        case Full(invoiceArticle) =>
          println("Found InvoiceArticle: "+invoiceArticle.articleType.get)
          invoiceItem.markInvoiced.addMarker(invoice.id.get, generatedInvoice.invoiceNo.get,
              invoice.invoiceYear.get, invoice.invoiceDate.get)
          if (invoiceArticle.fulfillmentType.get == InvoiceFulfillmentType.YearlyInvoice) {
            invoiceItem.isCompleted(false)
          } else {
            invoiceItem.isCompleted(true)
          }
          if (invoiceArticle.articleType.get == InvoiceArticleType.Dynamic) {
            println("Dynamic Item, markValuesInvoiced")
            InvoiceItemGenerator.markValuesInvoiced(invoice, generatedInvoice, dataProvider, invoiceItem, invoiceArticle)
          }
        case _ =>
          invoiceItem.markInvoiced.addMarker(invoice.id.get, generatedInvoice.invoiceNo.get,
              invoice.invoiceYear.get, invoice.invoiceDate.get)
          invoiceItem.isCompleted(true)
      }
    })

  }

  def findAllGardens(clientId: ObjectId) = Garden where (_.clientId eqs clientId) orderAsc(_.id) fetch()

//  def multiSearch(user: User, clientId: ObjectId, search: String): List[Garden] = {
//    val re = findAllGardens(clientId) filter (g => {
//      val sb = new StringBuilder()
//      sb.append(g.garden_no.get).append(" ").append(g.gardenOwners.asHtml.toString).append(" ").append(g.idElectricMeter.get).
//      append(" ").append(g.idWaterMeter.get)
//      val re = sb.toString.toLowerCase.indexOf(search)
//      //println(s"Searching ${sb} for ${search}: ${re}")
//      re > -1
//    })
//    re
//  }


}

class Garden private() extends MongoIdRecord[Garden] with ClientId[Garden] with CreatedUpdated[Garden]
                       with Selectable[Garden]
                       with InvoiceItemContainer[Garden]
                       with Attachments[Garden]
                       with MongoDataSourceTrait[Garden] {
  def meta = Garden

  def getClientId = this.clientId.get
  def uploadDoneCallback = GardenModalDialog.uploadDoneCallback
  override def isDocumentHidden = false

  val datasourceModule = "garden"

  def setAddInvoiceItemMarker = GardenModalDialog.AddInvoiceItem(true)

  object garden_no extends BsIntField(this) {
    override def validations = meta.uniqueValue(this, "Nummer ist nicht eindeutig") _ :: super.validations
    override def displayName = "Gartennummer"
  }

  object garden_size extends BsIntField(this) {
    override def displayName = "Fläche des Gartens (m2)"
  }

  object houseSize extends BsIntField(this) {
    override def displayName = "Fläche der Laube (m2)"
  }

  object idElectricMeter extends BsStringField(this, 50) {
    override def displayName = "Stromzähler Id"
  }

  object idWaterMeter extends BsStringField(this, 50) {
    override def displayName = "Wasserzähler Id"
  }

  object propertyTax extends BsObjectIdRefField(this, PropertyTax) {
    override def displayName = "Grundsteuer"
    override def asHtml = Text(obj.map(_.displayName) openOr "")

    override def options: List[(Box[ObjectId], String)] = {
      PropertyTax.findPropertyTaxes(clientId.get).
                 sortWith((a, b) => a.sortOrder.get < b.sortOrder.get).
                 map (r => (Full(r.id.get), r.displayName))
    }

    override def toForm: Box[NodeSeq] = {
      val opt = buildDisplayList.flatMap(r => r._1.map((_, r._2)))
      Full(SHtml.selectObj[ObjectId](opt, obj.map(_.id.get), v => this.set(v), "class" -> "form-control"))
    }
  }

  object section_1     extends BsStringField(this, 1000) {
    override def displayName = "Sektion 1"
  }

  object section_2     extends BsStringField(this, 1000) {
    override def displayName = "Sektion 2"
  }

  object section_3     extends BsStringField(this, 1000) {
    override def displayName = "Sektion 3"
  }

  object text_1        extends BsStringField(this, 1000) {
    override def displayName = "Freitext 1"
  }

  object text_2        extends BsStringField(this, 1000) {
    override def displayName = "Freitext 2"
  }

  object text_3        extends BsStringField(this, 1000) {
    override def displayName = "Freitext 3"
  }

  object text_4        extends BsStringField(this, 1000) {
    override def displayName = "Freitext 4"
  }

  object text_5        extends BsStringField(this, 1000) {
    override def displayName = "Freitext 5"
  }

  object text_6        extends BsStringField(this, 1000) {
    override def displayName = "Freitext 6"
  }

  object text_7        extends BsStringField(this, 1000) {
    override def displayName = "Freitext 7"
  }

  object text_8        extends BsStringField(this, 1000) {
    override def displayName = "Freitext 8"
  }

  object text_9        extends BsStringField(this, 1000) {
    override def displayName = "Freitext 9"
  }

  object waterMeterReading extends DateBoxedLongValueList(this) {
    override def displayName = "Zählerstand Wasser"
  }

  object electricityMeterReading extends DateBoxedLongValueList(this) {
    override def displayName = "Zählerstand Elektro"
  }

  object text_10       extends BsStringField(this, 1000) {
    override def displayName = "Freitext 10"
  }

  object gardenOwners  extends ReadOnlyProxyField[String] {
    override def displayName = "Pächter"
    def name = "owners"
    lazy val value = Membership.findFirstTwoGardenOwnerNames(id.get, ", ")
  }

  override def allRecordFields = gardenOwners :: (allFields: List[BaseField])

  override def getRepeatedElems(key: String): List[DataSource] = {
    key match {
      case "members"  => Garden.findMembersOfGarden(this)
      case _          => Nil
    }
  }

  override def clientGetRawProperty = {
    case "electricity_meter_diff"      => electricityMeterReading.deltaValueNotInvoiced
    case "electricity_charge_per_unit" => PropertyValue.findMoneyProperty(clientId.get, "electricity_charge_per_unit", PropertyValue.moneyValue.defaultValue)
    case "water_meter_diff"            => waterMeterReading.deltaValueNotInvoiced
    case "water_charge_per_unit"       => PropertyValue.findMoneyProperty(clientId.get, "water_charge_per_unit", PropertyValue.moneyValue.defaultValue)

  }

  override def markValueInvoiced(key: String, markInvoicedData: MarkInvoicedData): Unit = key match {
    case "electricity_meter_diff"      => electricityMeterReading.markValueInvoiced(markInvoicedData)
    case "water_meter_diff"            => waterMeterReading.markValueInvoiced(markInvoicedData)
    case _ =>
  }


}
