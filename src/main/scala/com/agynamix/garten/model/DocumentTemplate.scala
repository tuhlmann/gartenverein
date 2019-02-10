package com.agynamix.garten.model

import net.liftweb.common._
import net.liftweb.util.Helpers._
import com.agynamix.garten.model.share.ClientId
import com.agynamix.garten.lib.StructuredDbObject
import scala.xml.Text
import net.liftweb.mongodb.record.MongoMetaRecord
import com.agynamix.garten.model.share.CreatedUpdated
import com.agynamix.garten.model.share.MongoIdRecord
import net.liftweb.mongodb.BsonDSL._
import com.agynamix.garten.model.share.FormValidators
import org.bson.types.ObjectId
import com.foursquare.rogue.LiftRogue._
import net.liftweb.http.FileParamHolder
import net.liftweb.json._
import com.agynamix.garten.model.share.Attachments
import com.agynamix.garten.snippet.DocumentTemplateModalDialog
import com.agynamix.garten.model.share.TemplateProvider
import java.net.URL
import com.agynamix.garten.api.AllowedTypes
import com.agynamix.garten.lib.field.BsObjectIdRefField
import com.agynamix.garten.lib.field.BsEnumNameField
import com.agynamix.garten.lib.field.BsStringField
import com.agynamix.garten.lib.field.BsTextareaField
import com.agynamix.garten.lib.field.BsIntField
import net.liftweb.record.field.OptionalStringField
import java.io.InputStream
import net.liftweb.http.LiftRules

case class DocumentResource(version: Int, name: String, filename: String, displayName: String, identifier: String, tplType: DocumentType.Value, note: String = "") {
  def getStream: InputStream = {
    (for(url <- LiftRules.getResource("/documents/"+filename)) yield {
      url.openStream()
    }).openOrThrowException("Resource not found: "+filename)
  }
}

object DocumentTemplate extends DocumentTemplate with MongoMetaRecord[DocumentTemplate] with StructuredDbObject[DocumentTemplate]
                        with FormValidators[DocumentTemplate] with Loggable {

  lazy val StdInvitation = DocumentResource(3, "Einladung in den Verein", "invitation_letter.odt", "Standard Invitation Letter", "std_invitation_letter",
                           DocumentType.BulkLetter, "Wird versendet beim Einladen eines Mitgliedes in die Vereinssoftware")

  lazy val StdFormLetter = DocumentResource(2, "Anschreiben", "form_letter.odt", "Standard Form Letter", "std_form_letter",
                           DocumentType.BulkLetter, "Eine Vorlage für Formschreiben")

  lazy val StdInvoice    = DocumentResource(3, "Rechnung", "std_invoice.odt", "Standard Invoice Template", "std_invoice",
                           DocumentType.Invoice, "Eine Vorlage für Rechnungen")

  lazy val StdDocuments = StdInvitation :: StdFormLetter :: StdInvoice :: Nil

  def ensureStdDocumentUploadForClient(owner: User, client: Client) {
    for (document <- StdDocuments) {
      ensureStdDocumentUploadForClient(owner, client, document)
    }
  }

  def ensureStdDocumentUploadForClient(owner: User, client: Client, document: DocumentResource) {
    logger.info(s"Ensure document ${document.filename} for client ${client.name.get}")
    findByIdentifier(client.id.get, document.identifier) match {
      case Some(existing) =>
        if (existing.version.get < document.version) {
          logger.info(s"Update document ${document.identifier} to version ${document.version}")
          replaceDocument(existing, document)
        }
      case _ =>
        logger.info(s"Create Standard document ${document.identifier}")
        createStdDocumentTemplate(owner, client, document)
    }
  }


  def keyFields(user: User, args: Any*): Box[() => JObject] =
    user.activeMembership.activeClient.map(client => () => (clientId.name -> client.id.get))

  def createInstance(user: User, client: Client) = {
    DocumentTemplate.createRecord.author(user.id.get).clientId(client.id.get)
  }

  /**
   * Takes a document resource, uploads the file and creates a DocumentTemplate instance
   */
  def createStdDocumentTemplate(user: User, client: Client, document: DocumentResource): Box[DocumentTemplate] = {
    for (docInfo <- Document.documentService.save(List(client.id.get.toString, user.id.get.toString), document.getStream, document.filename, document.displayName)) yield {
      val doc = Document.createInstance(user.id.get, user.id.get, client.id.get, docInfo).hidden(true).save(true)
      val tpl = createInstance(user, client).templateType(document.tplType).
                version(document.version).documentIdentifier(document.identifier).
                name(document.name).attachments(List(doc.id.get)).note(document.note).save(true)
      tpl
    }
  }

  def replaceDocument(documentTemplate: DocumentTemplate, document: DocumentResource): Box[DocumentTemplate] = {
    for (existingDocument <- documentTemplate.attachments.objs.headOption) yield {
      Document.documentService.replaceFile(existingDocument.relativeFilePath, document.getStream)
      documentTemplate.version(document.version).name(document.name).note(document.note).save(true)
    }
  }

  def findByClient(clientId: ObjectId, templateType: DocumentType.Value) = {
    //println(s"Find tpl for client ${clientId} with type: ${templateType}")
    val query = DocumentTemplate where (_.clientId eqs clientId) and (_.templateType eqs templateType)
    //println("Query: "+query.toString)
    val re = (query fetch)
    //println("Found: "+re.toString())
    re
  }

  def findByIdentifier(clientId: ObjectId, identifier: String) = {
    DocumentTemplate where (_.clientId eqs clientId) and (_.documentIdentifier eqs identifier) get()
  }

  def defaultTemplate(clientId: ObjectId, templateType: DocumentType.Value): Box[DocumentTemplate] =
    findByClient(clientId, templateType).headOption

}

class DocumentTemplate private () extends MongoIdRecord[DocumentTemplate] with ClientId[DocumentTemplate]
                      with CreatedUpdated[DocumentTemplate] with Attachments[DocumentTemplate]
                      with TemplateProvider with Loggable {

  def meta = DocumentTemplate

  def getClientId = this.clientId.get
  def uploadDoneCallback = DocumentTemplateModalDialog.uploadDoneCallback
  override def maxAttachments = 1

  override def mimeTypeAllowed(fph: FileParamHolder): Option[String] = {
    AllowedTypes.openDocumentAllowed(fph)
  }

  def templateUrl: Box[URL] = {
    attachments.objs.headOption.map(_.documentUrl)
  }

  def getSubject(): String = name.get.trim

  object author extends BsObjectIdRefField(this, User) {
    override def displayName = "Autor"
    override def asHtml = Text(obj.map(_.displayName).openOr(""))
  }

  object templateType extends BsEnumNameField(this, DocumentType) {
    override def displayName = "Vorlagentyp"

    override def buildDisplayList = DocumentType.buildDisplayList(enum.values.toList)
    override def asHtml = DocumentType.asHtml(get)
  }

  /**
   * Only used for standard documents that have a fixed identifier
   */
  object documentIdentifier extends OptionalStringField(this, 100)


  object name extends BsStringField(this, 255) {
    override def validations = valMinLen(3, "Bitte geben Sie eine Bezeichnung an.") _ :: super.validations
    override def displayName = "Bezeichnung"
  }

  object note extends BsTextareaField(this, 10000) {
    override def displayName = "Beschreibung"
    override def textareaRows  = 3
  }

  object version extends BsIntField(this) {
    override def displayName = "Version"
    override def defaultValue = 1
  }


}