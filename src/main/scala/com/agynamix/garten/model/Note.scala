package com.agynamix.garten.model

import net.liftweb.common._
import net.liftweb.util.Helpers._
import com.agynamix.garten.model.share._
import com.agynamix.garten.lib.StructuredDbObject
import com.agynamix.garten.lib.field.GermanDateField
import scala.xml.Text
import java.util.Date
import net.liftweb.mongodb.BsonDSL._
import com.agynamix.garten.lib.field.DateFormAdapterField
import com.agynamix.garten.snippet.NoteModalDialog
import net.liftweb.mongodb.record.field.DateField
import com.agynamix.garten.lib.field.BsObjectIdRefField
import com.agynamix.garten.lib.field.BsStringField
import com.agynamix.garten.lib.field.BsHtmlAreaField
import net.liftweb.json.JsonAST.JObject
import org.bson.types.ObjectId
import com.foursquare.rogue.LiftRogue._


object Note extends Note with LogbookMetaRecord[Note] with StructuredDbObject[Note] with FormValidators[Note] {

  def keyFields(user: User, args: Any*): Box[() => JObject] =
    user.activeMembership.activeClient.map(client => () => (clientId.name -> client.id.get): JObject)

  def createInstance(user: User, client: Client): Note = {
    Note.createRecord.author(user.id.get).clientId(client.id.get)
  }

  def findMailOnlyRecipients(note: Note): List[RecipientData] = {
    note.recipients.obj.map(_.findEmailRecipients(note.author.get, note)).openOr(Nil)
  }

  def findNotesOrderByUpdate(user: User, clientId: ObjectId, maxItems: Int): List[Note] = {
    Note where (_.clientId eqs clientId) orderDesc (_.updatedAt) limit(maxItems) fetch
  }

}

class Note private () extends MongoIdRecord[Note] with ClientId[Note]
                      with CreatedUpdated[Note] with Attachments[Note]
                      with Recipients[Note] with Loggable {
  def meta = Note

  def getClientId = this.clientId.get
  def uploadDoneCallback = NoteModalDialog.uploadDoneCallback

  object author extends BsObjectIdRefField(this, User) {
    override def displayName = "Autor"
    override def asHtml = Text(asString)
    override def asString = obj.map(_.displayName).openOr("")
  }

  object noteCreated extends DateField(this) with DateFormAdapterField with GermanDateField {
    override def displayName = "Erstellt am"
    override def defaultValue = new Date()
  }

  object subject extends BsStringField(this, 255) {
    override def validations = valMinLen(3, "Bitte geben Sie einen Betreff an.") _ :: super.validations
    override def displayName = "Betreff"
  }

  object note extends BsHtmlAreaField(this, 10000) {
    override def displayName = "Notiz"
  }

}