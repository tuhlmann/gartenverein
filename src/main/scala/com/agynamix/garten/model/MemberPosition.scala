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

object MemberPosition extends MemberPosition with MongoMetaRecord[MemberPosition] with StructuredDbObject[MemberPosition] with FormValidators[MemberPosition] {

  def keyFields(user: User, args: Any*): Box[()=>JObject] =
    user.activeMembership.clientId.map(id => ()=>{(clientId.name -> id)})

  def createInstance(client: Client) = MemberPosition.createRecord.clientId(client.id.get)

  def createPosition(client: Client, name: String, sortOrder: Int) = createInstance(client).name(name).sortOrder(sortOrder)

  def createAndSavePosition(client: Client, name: String, sortOrder: Int) = createPosition(client, name, sortOrder).save(true)

  def findMemberPositions(clientId: ObjectId): List[MemberPosition] = MemberPosition where (_.clientId eqs clientId) fetch

  def findOrCreateStandardPositions(client: Client): List[MemberPosition] = {
    findMemberPositions(client.id.get) match {
      case Nil =>
        createAndSavePosition(client, "Anderes", 1) ::
        createAndSavePosition(client, "Erster Vorstand", 2) ::
        createAndSavePosition(client, "Zweiter Vorstand", 3) ::
        createAndSavePosition(client, "Schriftführer", 10) ::
        createAndSavePosition(client, "Bauleiter", 10) ::
        createAndSavePosition(client, "Beisitzer", 10) ::
        createAndSavePosition(client, "Kulturbeauftragter", 10) ::
        createAndSavePosition(client, "Rechnungsprüfer", 10) ::
        createAndSavePosition(client, "Wasserkommission", 10) ::
        createAndSavePosition(client, "Stromkommission", 10) :: Nil
      case l => l
    }
  }

  def deleteForClient(clientId: ObjectId) =
    (MemberPosition where (_.clientId eqs clientId) fetch) foreach (_.delete_!)

}

class MemberPosition private() extends MongoIdRecord[MemberPosition] with ClientId[MemberPosition] with CreatedUpdated[MemberPosition] {
  def meta = MemberPosition

  object name extends BsStringField(this, 100) {
    override def displayName = "Position im Verein"
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
