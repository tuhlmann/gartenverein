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
import net.liftweb.mongodb.record.field.ObjectIdRefListField
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
import net.liftmodules.widgets.bootstrap.Bs3ConfirmRemoveDialog
import net.liftweb.mongodb.record.field.UUIDField
import java.io.File
import java.util.UUID
import com.agynamix.garten.api.FileUploadInProgress
import net.liftweb.http.js.JE.AnonFunc
import com.agynamix.garten.snippet.StructuredLiftScreen
import com.agynamix.garten.snippet.StructuredLiftScreen
import com.agynamix.garten.snippet.NoteModalDialog
import com.agynamix.garten.lib.field.ExpiresField
import com.agynamix.garten.lib.field.BsObjectIdRefField
import com.agynamix.garten.lib.field.BsStringField
import com.agynamix.garten.lib.field.BsLongField
import com.agynamix.garten.lib.field.BsStringField
import com.agynamix.garten.lib.field.BsBooleanField
import net.liftweb.record.field.OptionalStringField
import org.apache.commons.io.IOUtils

object Document extends Document with MongoMetaRecord[Document] with StructuredDbObject[Document] with Loggable {

  lazy val documentService = GardenConfig.documentService.vend

  ensureIndex((guid.name -> 1), true)

  lazy val highDate = DateHelpers.germanShortDate.parse("31.12.9999")

  def keyFields(user: User, args: Any*): Box[() => JObject] =
    user.activeMembership.clientId.map(id => () =>
      (userId.name -> user.id.get) ~ (clientId.name -> id) ~ (hidden.name -> false)
    )

  def createInstance(userId: ObjectId, authorId: ObjectId, clientId: ObjectId, docInfo: DocumentInfo) =
    Document.createRecord.userId(userId).author(authorId).clientId(clientId).
                     fileName(docInfo.fileName).displayName(docInfo.displayName).mimeType(docInfo.mimeType).
                     length(docInfo.length).setRelativeFilePath(docInfo.filePath)

  def createInstance(user: User, clientId: ObjectId) = Document.createRecord.userId(user.id.get).clientId(clientId)

  def findDocument(documentGuid: String): Box[Document] = {
    try {
      Document.find(guid.name, UUID.fromString(documentGuid))
    } catch {
      case e: Exception => Empty
    }
  }

  /**
   * Remove the document from storage and delete the record
   */
  def removeDocuments(ids: List[ObjectId]): Unit = {
    Document.findAll(ids).foreach(doc => {
      Document.documentService.remove(doc.relativeFilePath)
      doc.delete_!
    })
  }

}

class Document private () extends MongoIdRecord[Document] with UserId[Document] with ClientId[Document] with CreatedUpdated[Document] with Loggable {
  def meta = Document

  object guid extends UUIDField(this)

  object expires extends ExpiresField(this) {
    override def defaultValue = Document.highDate
  }

  object hidden extends BsBooleanField(this) {
    override def defaultValue = false
  }

  object author extends BsObjectIdRefField(this, User) {
    override def displayName = "Autor"
    override def asHtml = Text(obj.map(_.displayName).openOr(""))
  }

  object fileName extends BsStringField(this, 200) {
	  override def displayName = "Dateiname"
  }

  object displayName extends BsStringField(this, 200) {
    override def displayName = "Name"
  }

  object mimeType extends BsStringField(this, 200) {
    override def displayName = "Mimetyp"
  }

  object length extends BsLongField(this, 200) {
    override def displayName = "Größe"
  }

  /**
   * Thats the name of the file, created from a UUID
   */
  object fileGuid     extends BsStringField(this, 100)

  object relativePath extends BsStringField(this, 300)

  def relativeFilePath = relativePath.get + File.separator + fileGuid.get
  def documentUrl = Document.documentService.documentUrl(relativeFilePath)

  def downloadDocumentUrl() = Document.documentService.downloadPath(guid.get.toString())

  def setRelativeFilePath(pathWithFilename: String): Document = {
    val splitPos = pathWithFilename.lastIndexOf(File.separator)
    val (path, name) = if ((splitPos > -1) && (pathWithFilename.length() > splitPos+1)) {
      (pathWithFilename.substring(0, splitPos), pathWithFilename.substring(splitPos + 1))
    } else ("", pathWithFilename)
    relativePath(path)
    fileGuid(name)
    this
  }

  def getBytes: Box[Array[Byte]] = {
    tryo(IOUtils.toByteArray(documentUrl.openStream()))
  }

}

trait DocumentForm[OwnerType <: MongoIdRecord[OwnerType]] extends ObjectIdRefListField[OwnerType, Document] {

  self: ObjectIdRefListField[OwnerType, Document] =>

  def getClientId: ObjectId
  def uploadDoneCallback: JsCmd
  def maxAttachments: Int
  def markDocumentForRemoval(doc: Document): Unit
  def isDocumentMarkedForRemoval(doc: Document): Boolean
  def isDocumentHidden: Boolean

  def addDocument(guid: String): Boolean = {
    (for (doc <- Document.findDocument(guid)) yield {
      //println("Add Document: "+doc)
      set(doc.id.get :: get)
      true
    }) openOr false
  }

  override def asHtml = {
    val docs = attachedDocuments(Empty, false)
    if (docs == NodeSeq.Empty) Text("Keine") else docs
  }

  override def toForm =
    uniqueFieldId match {
      case Full(id) => Full(elem(id))
      case _ => Full(elem(nextFuncName))
    }

  def mimeTypeAllowed(fph: FileParamHolder): Option[String] = Some(fph.mimeType)

  def elem(id: String) = {
    uploadedDocsForm(id, fph => {
                          for {mimeType <- mimeTypeAllowed(fph)
                               user <- User.currentUser
                               docInfo <- Document.documentService.save(List(getClientId.toString, user.id.get.toString), fph)} {
                            FileUploadInProgress.set(true) // This is to notify the form not to trigger a finish, but a refresh
                            val doc = Document.createInstance(user.id.get, user.id.get, getClientId, docInfo).hidden(isDocumentHidden).save(true)
                            set(doc.id.get :: get)
                          }
                        })

  }

  def uploadedDocsForm(elemId: String, func: FileParamHolder => Any, attrs: ElemAttr*): Elem = {
    val f2: FileParamHolder => Any = fp => if (fp.length > 0) func(fp)
    S.fmapFunc(BinFuncHolder(f2)) { name =>
      val id = nextFuncName
      val doneFunc = AnonFunc(uploadDoneCallback)
      val uploadBtnId = nextFuncName
      attrs.foldLeft(
        <div id={elemId}>
          <div id={uploadBtnId} style={if (get.size >= maxAttachments) "display:none" else ""}>
            <span class="btn btn-info fileinput-button form-control">
              <i class="icon-plus icon-black"></i>
              <span>{S ? "document.upload.form.button"}</span>
              <input class="jq-fileupload" type="file" name={ name } multiple="multiple"/>
            </span>
            <br/>
            <div id={ "prg_" + id } style="width:50%;" class="progress progress-striped">
              <div class="bar"></div>
            </div>
          </div>
          <div id={ "files_" + id }>{attachedDocuments(Full(uploadBtnId))}</div>
          <div name="jq-fileupload-script">
            { Script(OnLoad(Run("App.views.common.Upload.initFileupload('#%s .jq-fileupload', '%s', %s)".format(elemId, id, doneFunc.toJsCmd)))) }
          </div>
        </div>) { _ % _ }
    }
  }

  def downloadDocumentUrl(doc: Document): String = doc.downloadDocumentUrl()

  def doRemoveDocument(doc: Document): Boolean = {
    if (Document.documentService.remove(doc.relativeFilePath)) {
      set(objs.filterNot(_.guid.get == doc.guid.get).map(_.id.get))
      true
    } else false
  }

  def attachedDocuments(uploadBtnId: Box[String], isEditable: Boolean = true): NodeSeq = {
    def confirmDeleteDocument(elemId: String, doc: Document): JsCmd = {
      Bs3ConfirmRemoveDialog("Dokument löschen",
                          s"Möchten Sie das Dokument '${doc.fileName.get}' tatsächlich löschen?",
                          ()=> { markDocumentForRemoval(doc)
                                 Run("$('#%s').fadeOut('fast').remove()".format(elemId)) &
                                 uploadBtnId.map(id => Run("$('#%s').fadeIn('fast')".format(id))).openOr(Noop)
                          }, "backdrop" -> false)
    }

    def asTableRow(doc: Document): Elem = {
      val id = nextFuncName
      <tr id={id}>
        <td>{doc.createdAt.asHtml}</td>
        <td><a href={downloadDocumentUrl(doc)} target="garten_docs" >{doc.fileName.get}</a></td>
        <td>{doc.userId.obj.map(_.displayName).openOr("")}</td>
        {if (isEditable)
          <td class="action-td"><a href="Javascript://" onclick={SHtml.ajaxInvoke(()=>{confirmDeleteDocument(id, doc)})._2.toJsCmd}><i class="icon-remove"></i></a></td>
        }
      </tr>
    }
    if (objs.filterNot(isDocumentMarkedForRemoval).nonEmpty) {
      <table class="table table-condensed table-striped">
        <thead>
          <tr><td>Hinzugefügt</td><td>Name</td><td>Autor</td>{if (isEditable) <td></td>}</tr>
        </thead>
        <tbody> {objs.filterNot(isDocumentMarkedForRemoval).sortBy(_.createdAt.get).map(asTableRow)} </tbody>
      </table>
    } else NodeSeq.Empty
  }


}
