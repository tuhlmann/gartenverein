package com.agynamix.invoice.model

import com.agynamix.garten.lib.StructuredDbObject
import com.agynamix.garten.model.share.ClientId
import com.agynamix.garten.model.share.UserId
import com.agynamix.garten.model.User
import net.liftweb.mongodb.record.MongoMetaRecord
import com.agynamix.garten.model.share.CreatedUpdated
import com.agynamix.garten.model.share.MongoIdRecord
import net.liftweb.record.field.StringField
import net.liftweb.json.JsonAST.JObject
import net.liftweb.mongodb.BsonDSL._
import net.liftweb.common._
import com.agynamix.garten.model.share.FormValidators
import com.agynamix.garten.model.Client
import com.agynamix.garten.lib.field.BsEnumNameField
import com.agynamix.garten.lib.field.BsStringField
import com.agynamix.garten.lib.util.LocalizedEnum
import scala.xml.Node
import org.bson.types.ObjectId
import com.foursquare.rogue.LiftRogue._
import com.agynamix.garten.lib.field.BsTextareaField
import com.agynamix.garten.lib.field.BsVatField
import com.agynamix.garten.lib.field.BsBooleanField
import com.agynamix.garten.lib.field.BsIntField
import com.agynamix.garten.lib.field.BsMoneyField
import scala.xml.Text

object InvoiceArticleType extends Enumeration with LocalizedEnum {
  type InvoiceArticleType = Value

  val locKeyBase = "InvoiceArticle.type"

  val Manual       = Value("manual")
  val Dynamic      = Value("dynamic")
  val Article      = Value("article")

}


object InvoiceArticle extends InvoiceArticle with MongoMetaRecord[InvoiceArticle] with StructuredDbObject[InvoiceArticle] with FormValidators[InvoiceArticle] {

  def keyFields(user: User, args: Any*): Box[()=>JObject] =
    user.activeMembership.clientId.map(id => ()=>(clientId.name -> id))

  def createInstance(user: User, client: Client) = InvoiceArticle.createRecord.userId(user.id.get).clientId(client.id.get)


  def findByArticleType(clientId: ObjectId, articleType: InvoiceArticleType.Value) =
    InvoiceArticle where (_.clientId eqs clientId) and (_.articleType eqs articleType) fetch

}


class InvoiceArticle private() extends MongoIdRecord[InvoiceArticle] with UserId[InvoiceArticle] with ClientId[InvoiceArticle] with CreatedUpdated[InvoiceArticle] {
  def meta = InvoiceArticle

  object identifier extends BsStringField(this, 100) {
    override def validations = valMinLen(3, "Bitte geben Sie einen Namen an.") _ ::
                               meta.uniqueValue(this, "Name ist nicht eindeutig") _ ::
                               super.validations

    override def displayName = "Bezeichnung"
  }

  object articleType extends BsEnumNameField(this, InvoiceArticleType) {
    override def displayName = "Artikeltyp"
    //override def toFormAppendedAttributes = new UnprefixedAttribute("disabled", "disabled", super.toFormAppendedAttributes)

    override def buildDisplayList = InvoiceArticleType.buildDisplayList(enum.values.toList.filterNot(_ == InvoiceArticleType.Manual))
    override def asHtml = InvoiceArticleType.asHtml(get)
  }

  object fulfillmentType extends BsEnumNameField(this, InvoiceFulfillmentType) {
    override def displayName = "Art der Rechnung"
    override def buildDisplayList = InvoiceFulfillmentType.buildDisplayList(enum.values.toList)
    override def asHtml = InvoiceFulfillmentType.asHtml(get)
    override def helpAsHtml = Full(Text("Art der Rechnung. Artikel vom Typ 'Jahresrechnung' werden jedes Jahr wieder in Rechnung gestellt."))
  }

  object description extends BsTextareaField(this, 1000) {
    override def displayName = "Beschreibung"
    override def textareaRows = 3
  }

  object vat extends BsVatField(this) {
    def getClientId = owner.clientId.get
    override def displayName = "MwSt."
  }

  object doForeach extends BsStringField(this, 100) {
    override def displayName = "Für jeden Datensatz von (opt)"
  }

  object unitFormula extends BsStringField(this, 200) {
    override def displayName = "Berechnung der Menge"
  }

  object unitType extends BsEnumNameField(this, InvoiceItemUnitType) {
    override def displayName = "Einheit"
    override def buildDisplayList = InvoiceItemUnitType.buildDisplayList(enum.values.toList)
    override def asHtml = InvoiceItemUnitType.asHtml(get)
  }

  /**
   * Used for regular articles
   */
  object amount extends BsMoneyField(this)

  /**
   * For dynamic articles there is a formula that describes how to calculate the article
   */
  object amountFormula extends BsStringField(this, 200) {
    override def displayName = "Preis pro Einheit"
  }

  object articleDescription extends BsTextareaField(this, 300) {
    override def displayName = "Artikelbeschreibung"
    override def textareaRows = 3
  }

}

