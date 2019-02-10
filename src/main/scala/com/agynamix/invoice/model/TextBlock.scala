package com.agynamix.invoice.model

import net.liftweb.common._
import com.agynamix.garten.lib.StructuredDbObject
import com.agynamix.garten.model.share.ClientId
import com.agynamix.garten.model.share.UserId
import com.agynamix.garten.model.User
import com.agynamix.garten.model.share.CreatedUpdated
import com.agynamix.garten.model.share.MongoIdRecord
import net.liftweb.record.field.TextareaField
import net.liftweb.record.field.StringField
import net.liftweb.mongodb.record.MongoMetaRecord
import net.liftweb.mongodb.BsonDSL._
import net.liftweb.json.JsonAST.JObject
import com.agynamix.garten.model.share.FormValidators
import com.agynamix.garten.model.Client
import com.agynamix.garten.lib.field.BsTextareaField
import com.agynamix.garten.lib.field.BsStringField


object TextBlock extends TextBlock with MongoMetaRecord[TextBlock] with StructuredDbObject[TextBlock] with FormValidators[TextBlock] {

//  def keyFields2(user: User, args: Any*): (String, List[Any]) =
//    ("%s = ? and %s = ?".format(userId.name, clientId.name), List(user.id.is, user.activeClientConnection.clientId))

  def keyFields(user: User, args: Any*): Box[()=>JObject] =
    user.activeMembership.clientId.map(id => ()=>(userId.name -> user.id.get) ~ (clientId.name -> id))

  def createInstance(user: User, client: Client) = TextBlock.createRecord.userId(user.id.get).clientId(client.id.get)

}


class TextBlock private() extends MongoIdRecord[TextBlock] with UserId[TextBlock] with ClientId[TextBlock] with CreatedUpdated[TextBlock] {
  def meta = TextBlock

  object identifier extends BsStringField(this, 100) {
    override def validations = valMinLen(3, "Bitte geben Sie einen Namen an.") _ ::
                               meta.uniqueValue(this, "Name ist nicht eindeutig") _ ::
                               super.validations

    override def displayName = "Bezeichnung"
  }

  object text extends BsTextareaField(this, 5000) {
    override def displayName = "Text"
  }

}