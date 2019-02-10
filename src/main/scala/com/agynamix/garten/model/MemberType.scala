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
import com.agynamix.garten.lib.field.BsBooleanField
import com.agynamix.garten.lib.field.BsMoneyField
import org.joda.money.Money
import org.joda.money.CurrencyUnit

object MemberType extends MemberType with MongoMetaRecord[MemberType] with StructuredDbObject[MemberType] with FormValidators[MemberType] {

  def keyFields(user: User, args: Any*): Box[()=>JObject] =
    user.activeMembership.clientId.map(id => ()=>{(clientId.name -> id)})

  def createInstance(client: Client) = MemberType.createRecord.clientId(client.id.get)

  def createType(client: Client, name: String, isContractor: Boolean, sortOrder: Int) =
    createInstance(client).name(name).isContractor(isContractor).sortOrder(sortOrder)

  def createAndSaveType(client: Client, name: String, isContractor: Boolean, sortOrder: Int) = createType(client, name, isContractor, sortOrder).save(true)

  def findMemberTypes(clientId: ObjectId): List[MemberType] = MemberType where (_.clientId eqs clientId) fetch

  def findOrCreateStandardTypes(client: Client): List[MemberType] = {
    findMemberTypes(client.id.get) match {
      case Nil =>
        createAndSaveType(client, "Anderes", false, 1) ::
        createAndSaveType(client, "Hauptmitglied", true, 2) ::
        createAndSaveType(client, "Fördermitglied", false, 3) ::
        createAndSaveType(client, "Ehegattenmitglied", false, 4) :: Nil
      case l => l
    }
  }


  def deleteForClient(clientId: ObjectId) =
    (MemberType where (_.clientId eqs clientId) fetch) foreach (_.delete_!)

}

class MemberType private() extends MongoIdRecord[MemberType] with ClientId[MemberType] with CreatedUpdated[MemberType] {
  def meta = MemberType

  object name extends BsStringField(this, 100) {
    override def displayName = "Art der Mitgliedschaft"
  }

  object isContractor extends BsBooleanField(this) {
    override def displayName = "Ist Vertragspartner"
    override def defaultValue = false
  }

  object yearlyPayment extends BsMoneyField(this) {
    override def displayName = "Jährliche Gebühr"
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
