package com.agynamix.garten
package model

import com.agynamix.garten.lib.field._
import net.liftweb._
import mongodb.record._
import mongodb.record.field._
import record.field.StringField
import org.bson.types.ObjectId
import net.liftweb.http.S
import com.foursquare.rogue.LiftRogue._
import com.agynamix.garten.config.RolesDef
import com.agynamix.garten.config.Permissions
import com.agynamix.garten.model.share._
import com.agynamix.garten.lib.StructuredDbObject
import net.liftweb.record.field.BooleanField
import scala.xml.Text
import scala.xml.NodeSeq
import net.liftweb.mongodb.BsonDSL._
import net.liftweb.common._
import net.liftweb.json.JsonAST._
import net.liftweb.util.Helpers._
import net.liftweb.record.field.OptionalStringField
import net.liftweb.http.S.SFuncHolder
import com.agynamix.garten.lib.util.SnippetHelpers
import com.agynamix.garten.lib.Permission
import net.liftweb.record.field.IntField
import com.agynamix.garten.snippet.DocumentFoldersModalDialog
import net.liftweb.json.JsonAST.JObject
import net.liftweb.json.JsonAST.JObject
import net.liftweb.json.JsonAST.JObject
import java.util.Date
import net.liftweb.json.JsonAST.JObject
import com.agynamix.garten.lib.util.LocalizedEnum
import net.liftweb.http.SHtml

object DocumentFolderIconType extends Enumeration with LocalizedEnum {
  type DocumentFolderIconType = Value

  val locKeyBase = "documentfolder.icon.type"

  val MyDocuments = Value("my_documents")
  val OtherFolder = Value("other")

  def displayValue(t: DocumentFolderIconType.Value) = localized(t)
  
  def iconName(t: DocumentFolderIconType.Value) = {
    t match {
      case DocumentFolderIconType.MyDocuments => "icon-cloud"
      case DocumentFolderIconType.OtherFolder => "icon-folder-open-alt"
    }
  }

}


object DocumentFolder extends DocumentFolder with MongoMetaRecord[DocumentFolder] with StructuredDbObject[DocumentFolder] with SnippetHelpers {

  def keyFields(user: User, args: Any*): Box[()=>JObject] =
    user.activeMembership.clientId.map(id => ()=>(clientId.name -> id): JObject)

  def createInstance(user: User, client: Client): DocumentFolder =
    DocumentFolder.createRecord.author(user.id.get).clientId(client.id.get)


}

/*
 * Simple record for storing roles. Role name is the PK.
 */
class DocumentFolder private() extends MongoIdRecord[DocumentFolder]
                                  with ClientId[DocumentFolder]
                                  with CreatedUpdated[DocumentFolder]
                                  with Recipients[DocumentFolder]
                                  with Attachments[DocumentFolder]
                                  with SnippetHelpers {
  def meta = DocumentFolder

  def getClientId = this.clientId.get
  def uploadDoneCallback = DocumentFoldersModalDialog.uploadDoneCallback

  object author extends BsObjectIdRefField(this, User) {
    override def displayName = "Autor"
    override def asHtml = Text(asString)
    override def asString = obj.map(_.displayName).openOr("")
  }

  object folderCreated extends DateField(this) with DateFormAdapterField with GermanDateField {
    override def displayName = "Erstellt am"
    override def defaultValue = new Date()
  }

  object title extends BsStringField(this, 255) {
    override def validations = valMinLen(3, "Bitte geben Sie einen Titel an.") _ :: super.validations
    override def displayName = "Titel"
  }

  object note extends BsTextareaField(this, 10000) {
    override def displayName = "Notizen"
    override def textareaRows = 4
  }
  
  object folderIconType extends BsEnumNameField(this, DocumentFolderIconType) {
    override def displayName = "Typ"
    override def defaultValue = DocumentFolderIconType.OtherFolder

    override def buildDisplayList = {
      DocumentFolderIconType.buildDisplayList(enum.values.toList)
    }
    override def asHtml = {
      val icon = DocumentFolderIconType.iconName(get)
      <i class={icon}></i>
    }
  }
  

}
