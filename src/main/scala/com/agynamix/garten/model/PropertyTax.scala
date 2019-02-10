package com.agynamix.garten.model

import net.liftweb.common._
import com.agynamix.garten.lib.StructuredDbObject
import com.agynamix.garten.model.share.ClientId
import com.agynamix.garten.model.share.UserId
import net.liftweb.mongodb.record.MongoMetaRecord
import com.agynamix.garten.model.share.MongoIdRecord
import com.agynamix.garten.model.share.CreatedUpdated
import net.liftweb.record.field.StringField
import net.liftweb.record.field.IntField
import net.liftweb.json.JsonAST.JObject
import net.liftweb.mongodb.BsonDSL._
import com.foursquare.rogue.LiftRogue._
import com.agynamix.garten.model.share.FormValidators
import net.liftweb.record.field.DateTimeField
import net.liftweb.mongodb.record.field.DateField
import com.agynamix.garten.lib.field.BsDateField
import java.util.Date
import net.liftweb.record.field.BooleanField
import java.text.SimpleDateFormat
import org.bson.types.ObjectId
import com.agynamix.garten.lib.field.BsEnumNameField
import com.agynamix.garten.lib.field.BsTextareaField
import com.agynamix.garten.lib.field.BsStringField
import scala.xml.Text
import scala.xml.NodeSeq
import com.agynamix.garten.lib.field.BsIntField

object PropertyTax extends PropertyTax with MongoMetaRecord[PropertyTax] with StructuredDbObject[PropertyTax] with FormValidators[PropertyTax] {

  def keyFields(user: User, args: Any*): Box[()=>JObject] =
    user.activeMembership.clientId.map(id => ()=>{(clientId.name -> id)})

  def createInstance(client: Client) = PropertyTax.createRecord.clientId(client.id.get)

  def createTax(client: Client, name: String, tax: Int, sortOrder: Int) = createInstance(client).name(name).tax(tax).sortOrder(sortOrder)

  def createAndSaveTax(client: Client, name: String, tax: Int, sortOrder: Int) = createTax(client, name, tax, sortOrder).save(true)

  def findPropertyTaxes(clientId: ObjectId): List[PropertyTax] = PropertyTax where (_.clientId eqs clientId) fetch

  def findOrCreateStandardTaxes(client: Client): List[PropertyTax] = {
    findPropertyTaxes(client.id.get) match {
      case Nil =>
        createAndSaveTax(client, "A", 10, 1) ::
        createAndSaveTax(client, "B", 15, 2) :: Nil
      case l => l
    }
  }


  def deleteForClient(clientId: ObjectId) =
    (PropertyTax where (_.clientId eqs clientId) fetch) foreach (_.delete_!)

}

class PropertyTax private() extends MongoIdRecord[PropertyTax] with ClientId[PropertyTax] with CreatedUpdated[PropertyTax] {
  def meta = PropertyTax

  object name extends BsStringField(this, 100) {
    override def displayName = "Bezeichnung"
  }

  object tax extends BsIntField(this) {
    override def displayName = "Steuersatz (%)"
  }

  object sortOrder extends BsIntField(this) {
    override def displayName = "Sortierung"
    override def defaultValue = 1
  }

  object note  extends BsTextareaField(this, 1000) {
    override def displayName = "Bemerkung"
  }

  def displayName = name.get

}
