package com.agynamix.invoice.model

import net.liftweb.common._
import com.agynamix.garten.lib.StructuredDbObject
import com.agynamix.garten.model.share.ClientId
import com.agynamix.garten.model.share.UserId
import com.agynamix.garten.model.User
import net.liftweb.mongodb.record.MongoMetaRecord
import com.agynamix.garten.model.share.MongoIdRecord
import com.agynamix.garten.model.share.CreatedUpdated
import net.liftweb.json.JsonAST.JObject
import net.liftweb.mongodb.BsonDSL._
import com.foursquare.rogue.LiftRogue._
import com.agynamix.garten.model.share.FormValidators
import com.agynamix.garten.model.Client
import net.liftweb.record.field.DateTimeField
import net.liftweb.mongodb.record.field.DateField
import com.agynamix.garten.lib.field.BsDateField
import java.util.Date
import java.text.SimpleDateFormat
import org.bson.types.ObjectId
import com.agynamix.garten.lib.field.BsEnumNameField
import com.agynamix.garten.lib.field.BsStringField
import com.agynamix.garten.lib.field.BsBooleanField
import com.agynamix.garten.lib.field.BsIntField

object VatType extends Enumeration {
  type VatType = Value

  val Vat_0             	= Value("vat_0")
  val Vat_7                 = Value("vat_7")
  val Vat_19                = Value("vat_19")
  val Manual        		= Value("manual_vat")
}

object Vat extends Vat with MongoMetaRecord[Vat] with StructuredDbObject[Vat] with FormValidators[Vat] {

  lazy val validFrom1970 = new Date(0L)
  lazy val validFrom2007 = new SimpleDateFormat("dd.MM.yyyy").parse("01.01.2007")


  def keyFields(user: User, args: Any*): Box[()=>JObject] =
    user.activeMembership.clientId.map(id => ()=>{(clientId.name -> id)})

  def createInstance(client: Client) = Vat.createRecord.clientId(client.id.get)

  def createVat(client: Client, name: String, vat: Int, validFrom: Date) =
      createInstance(client).name(name).vat(vat).valid_from(validFrom)

  def findOrCreateVat(client: Client, vatType: VatType.Value, name: String, vat: Int, validFrom: Date, editable: Boolean = true): Vat = {
    (Vat where (_.clientId eqs client.id.get) and (_.vatType eqs vatType) get()) getOrElse {
      createVat(client, name, vat, validFrom).vatType(vatType).editable(editable).save(true)
    }
  }

  def findOrCreateStandardVats(client: Client): List[Vat] = {
    findOrCreateVat(client, VatType.Vat_0 , "Ohne MwSt"      , 0 , validFrom1970, false) ::
    findOrCreateVat(client, VatType.Vat_7 , "Reduzierte MwSt", 7 , validFrom1970, false) ::
    findOrCreateVat(client, VatType.Vat_19, "Standard MwSt"  , 19, validFrom2007, false) :: Nil
  }

  def findClientVats(clientId: ObjectId): List[Vat] = {
    Vat where (_.clientId eqs clientId) fetch
  }

  def deleteForClient(clientId: ObjectId) =
    (Vat where (_.clientId eqs clientId) fetch) foreach (_.delete_!)

}


class Vat private() extends MongoIdRecord[Vat] with ClientId[Vat] with CreatedUpdated[Vat] {
  def meta = Vat

  object name extends BsStringField(this, 100) {
    override def displayName = "Bezeichnung"
  }

  object editable extends BsBooleanField(this) {
    override def defaultValue = true
  }

  object vatType extends BsEnumNameField(this, VatType) {
    override def defaultValue = VatType.Manual
  }

  object vat extends BsIntField(this) {
    override def displayName = "MwSt (%)"

    def percentToDouble: Double = {
      val bd = BigDecimal.int2bigDecimal(get) / 100
      bd.toDouble
    }

  }

  object valid_from extends BsDateField(this) {
    override def displayName = "Gültig Ab"
  }

}