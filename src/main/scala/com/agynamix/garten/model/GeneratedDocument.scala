package com.agynamix.garten.model

import net.liftweb.common._
import net.liftweb.util.Helpers._
import net.liftweb.util.FieldContainer
import com.agynamix.garten.model.share.UserId
import com.agynamix.garten.model.share.ClientId
import com.agynamix.garten.lib.StructuredDbObject
import net.liftweb.json.JsonAST.JObject
import com.agynamix.garten.lib.field.GermanDateField
import scala.xml.Text
import com.agynamix.garten.lib.util.DateHelpers
import net.liftweb.http.SHtml
import scala.xml.NodeSeq
import net.liftweb.http.S
import net.liftweb.http.S.SFuncHolder
import net.liftweb.http.js.JsCmds
import com.agynamix.garten.snippet.Gardens
import scala.xml.Elem
import java.util.Date
import net.liftweb.mongodb.record.MongoMetaRecord
import com.agynamix.garten.model.share.CreatedUpdated
import com.agynamix.garten.model.share.MongoIdRecord
import net.liftweb.record.field.IntField
import net.liftweb.mongodb.record.field.ObjectIdRefField
import net.liftweb.record.field.StringField
import net.liftweb.record.field.TextareaField
import net.liftweb.record.field.BooleanField
import net.liftweb.mongodb.BsonDSL._
import com.agynamix.garten.model.share.FormValidators
import java.text.SimpleDateFormat
import net.liftweb.json.DefaultFormats
import net.liftweb.record.LifecycleCallbacks
import java.util.Calendar
import com.agynamix.garten.lib.field.DateFormAdapterField
import org.bson.types.ObjectId
import com.foursquare.rogue.LiftRogue._
import net.liftweb.mongodb.record.field.BsonRecordListField
import net.liftweb.mongodb.record.BsonRecord
import net.liftweb.mongodb.record.BsonMetaRecord
import java.util.GregorianCalendar
import net.liftweb.http.js.JsCmds.Alert
import net.liftweb.http.js.JsCmd
import com.agynamix.garten.model.share.ProxyBsonField
import net.liftweb.mongodb.record.field.BsonRecordField
import net.liftweb.record.Field
import org.joda.time.DateTime
import org.joda.time.Years
import net.liftweb.http.FileParamHolder
import net.liftweb.http.SHtml.ElemAttr
import net.liftweb.http.S.BinFuncHolder
import net.liftweb.http.js.JsCmds._
import net.liftweb.record.TypedField
import net.liftweb.record.BaseField
import com.agynamix.garten.config.GardenConfig
import com.agynamix.garten.service.DocumentInfo
import net.liftweb.record.field.LongField
import net.liftweb.mongodb.record.field.UUIDField
import java.io.File
import java.util.UUID
import com.agynamix.garten.api.FileUploadInProgress
import net.liftweb.http.js.JE.AnonFunc
import com.agynamix.garten.snippet.StructuredLiftScreen
import com.agynamix.garten.snippet.StructuredLiftScreen
import com.agynamix.garten.snippet.NoteModalDialog
import net.liftweb.mongodb.record.field.ObjectIdRefListField
import net.liftweb.json._
import com.agynamix.garten.model.share.Schedulable
import net.liftweb.mongodb.record.field.DateField
import net.liftweb.mongodb.record.field.OptionalDateField
import scala.xml.Unparsed
import com.agynamix.garten.lib.field.BsOptionalDateTimeField
import com.agynamix.garten.model.share.Attachments
import com.agynamix.garten.snippet.DocumentTemplateModalDialog
import com.agynamix.garten.model.share.OptionalObjectIdRefField
import com.agynamix.garten.snippet.BulkLetterModalDialog
import com.agynamix.garten.model.share.Recipients
import java.net.URL
import com.agynamix.garten.model.share.TemplateProvider
import com.agynamix.garten.api.AllowedTypes
import com.agynamix.garten.lib.field.BsObjectIdRefField
import com.agynamix.garten.lib.field.BsStringField
import com.agynamix.garten.lib.field.BsTextareaField
import com.agynamix.garten.lib.field.BsEnumNameField
import com.agynamix.garten.lib.util.LocalizedEnum
import com.agynamix.garten.lib.field.BsObjectIdRefListField
import com.agynamix.garten.snippet.GeneratedDocumentModalDialog
import com.agynamix.garten.lib.field.BsDateField
import com.agynamix.garten.lib.field.BsBooleanField
import com.agynamix.garten.model.share.GeneratedDocumentsHolder

object DocumentType extends Enumeration with LocalizedEnum {
  type DocumentType = Value

  val locKeyBase = "generated.document.type"

  val BulkLetter        	= Value("bulk_letter")
  val Invoice               = Value("invoice")
}

object GenerationStatus extends Enumeration with LocalizedEnum {
  type GenerationStatus = Value

  val locKeyBase = "generated.document.status"

  val NotGenerated        	= Value("empty")
  val Processing            = Value("processing")
  val Processed             = Value("processed")
}

object DocumentDeliveryOptions extends Enumeration with LocalizedEnum {
  type DocumentDeliveryOptions = Value

  val locKeyBase = "bulk.delivery"

  val AllPostalMail       = Value("all_postal")
  val PostalAndAttachment = Value("postal_and_attachment")
  val PostalAndLink       = Value("postal_and_link")
}

object GeneratedDocument extends GeneratedDocument with MongoMetaRecord[GeneratedDocument] with StructuredDbObject[GeneratedDocument] with FormValidators[GeneratedDocument] {

  ensureIndex((documents.name -> 1), false)

  def keyFields(user: User, args: Any*): Box[() => JObject] =
    user.activeMembership.activeClient.map(client => () => (clientId.name -> client.id.get))

  def createInstance(user: User, client: Client) = {
    GeneratedDocument.createRecord.author(user.id.get).clientId(client.id.get)
  }

  def findRecipientMembers(doc: GeneratedDocument): List[Membership] = {
    doc.recipients.obj.map(_.findRecipientMembers(doc.author.get, doc)).openOr(Nil)
  }

  def findByDocumentId(documentId: ObjectId): Box[GeneratedDocument] = {
    GeneratedDocument where (_.documents contains documentId) get()
  }

  def removeReferencedDocuments(doc: GeneratedDocument): Unit = {
    Document.removeDocuments(doc.zipAllDocuments.get :: doc.zipUsersWithoutEmailDocuments.get :: doc.documents.get)
  }

  override def delete_!(doc: GeneratedDocument): Boolean = {
    removeReferencedDocuments(doc)
    super.delete_!(doc)
  }

}

class GeneratedDocument private () extends MongoIdRecord[GeneratedDocument] with ClientId[GeneratedDocument]
                      with CreatedUpdated[GeneratedDocument]
                      with Attachments[GeneratedDocument]
                      with Recipients[GeneratedDocument]
                      with GeneratedDocumentsHolder[GeneratedDocument]
                      with TemplateProvider with Loggable {

  def meta = GeneratedDocument

  def getClientId = this.clientId.get
  def uploadDoneCallback = documentType.get match {
    case DocumentType.BulkLetter => BulkLetterModalDialog.uploadDoneCallback
    case _                       => BulkLetterModalDialog.uploadDoneCallback
  }
  override def maxAttachments = 1

  override def mimeTypeAllowed(fph: FileParamHolder): Option[String] = AllowedTypes.openDocumentAllowed(fph)

  def templateUrl: Box[URL] = {
    templateRef.obj.flatMap(_.templateUrl) or {
      attachments.objs.headOption.map(_.documentUrl)
    }
  }

  def getSubject(): String = subject.get.trim

  object documentType extends BsEnumNameField(this, DocumentType) {
    override def defaultValue = DocumentType.BulkLetter

    override def buildDisplayList = DocumentType.buildDisplayList(enum.values.toList)
    override def asHtml = DocumentType.asHtml(get)
  }

  object author extends BsObjectIdRefField(this, User) {
    override def displayName = "Autor"
    override def asHtml = Text(asString)
    override def asString = obj.map(_.displayName).openOr("")
  }

  object templateRef extends OptionalObjectIdRefField(this, DocumentTemplate) {
    override def displayName = "Vorlage"

    def templates: List[DocumentTemplate] = DocumentTemplate.findByClient(owner.clientId.get, owner.documentType.get)
    override def asHtml = obj.map(_.name.asHtml) openOr Text("")

    override def options: List[(Box[ObjectId], String)] = templates.map(t => (Full(t.id.get), t.name.get))

    override def toForm: Box[NodeSeq] = {
      Full(SHtml.selectObj[Box[ObjectId]](buildDisplayList, Full(obj.map(_.id.get)), v => setBox(v), "class" -> "form-control"))
    }
  }

  object delivery extends BsEnumNameField(this, DocumentDeliveryOptions) {
    override def displayName = "Versand"

    override def buildDisplayList = DocumentDeliveryOptions.buildDisplayList(enum.values.toList)
    override def asHtml = DocumentDeliveryOptions.asHtml(get)
  }

  object subject extends BsStringField(this, 255) {
    override def displayName = "Betreff (opt)"
    override def optional_? = true
  }

  object note extends BsTextareaField(this, 10000) {
    override def displayName = "Text (opt)"
    override def textareaRows  = 4
  }

  object documentEditable extends BsBooleanField(this) {
    override def defaultValue = true
  }

  def isDocumentEditable: Boolean = documentEditable.get

}