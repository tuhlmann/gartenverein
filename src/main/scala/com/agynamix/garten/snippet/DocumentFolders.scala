package com.agynamix.garten.snippet

import reactive.Observing
import reactive.BufferSignal
import net.liftweb.http.SHtml._
import net.liftweb.http.js.JsCmds._
import net.liftweb.common._
import net.liftweb.util.Helpers._
import reactive.web.Repeater
import reactive.web.html.Button
import net.liftweb.sitemap._
import net.liftweb.sitemap.Loc._
import scala.xml.Text
import net.liftweb.http.RequestVar
import com.agynamix.garten.lib.util.DateHelpers
import com.agynamix.garten.lib.util.SnippetHelpers
import reactive._
import reactive.web._
import scala.xml.Elem
import net.liftweb.http.S
import com.agynamix.garten.model._
import net.liftweb.http.PaginatorSnippet
import scala.xml.NodeSeq
import net.liftweb.util.CssSel
import java.util.Date
import net.liftweb.http.FieldBinding
import com.agynamix.garten.lib.field.GenderType
import net.liftweb.http.js.JsCmd
import com.agynamix.garten.config.Permissions._
import com.agynamix.garten.config.Permissions
import net.liftweb.util.BaseField
import net.liftweb.http.js.JsCmds.Run
import com.agynamix.garten.api.FileUploadInProgress
import net.liftweb.http.js.JE.AnonFunc
import com.agynamix.garten.lib.MailSender
import net.liftweb.http.js.JsCmds.Run


object DocumentFolders extends StructuredMetaSnippet(DocumentFolder) {

  lazy val listMenuParam =
    Menu.param[DocumentFolder]("OneMemberRole", Loc.LinkText(a => Text(a.id.get.toString)), id => DocumentFolder.find(id),
      (obj: DocumentFolder) => obj.id.get.toString) / "management" / "library" / *

}

class DocumentFolders extends StructuredFormSnippet[DocumentFolder](DocumentFolders, DocumentFoldersModalDialog, DocumentFoldersDetailView) {

  override def overridenDispatch: DispatchIt = {
    case "listDashboard" => listDashboard
  }

  lazy val listPaginator = new OffsetPaginator(this) {

    implicit val manifest = Manifest.classType[DocumentFolder](Role.getClass)

    def masterTableFields = List(DbField(DocumentFolder.folderIconType, true, true, true),
                                 DbField(DocumentFolder.folderCreated, true, true, true),
                                 DbField(DocumentFolder.title, true, true, true),
                                 DbField(DocumentFolder.author, true, true, true))

    //                            DbField(Address.city, true, true, true,
    //                                   (f: BaseField, rec: Member) => rec.currentAddress.city.asHtml),
  }

  def listElement(user: User, selFunc: DocumentFolder=>JsCmd)(obj: DocumentFolder) = {
    listRecordElements(user, obj, selFunc) &
      "@action [onclick]" #> ajaxInvoke(()=>{
        val href = DocumentFolders.listMenuParam.calcHref(obj)
        RedirectTo(href)
      }) &
      bindEdit("@edit", obj) &
      bindRemove("@remove", obj)
  }

  def doBindRemoveAction(obj: DocumentFolder) = {
    ajaxRemoveAction(obj, "Dokumentenmappe löschen",
      "Möchten Sie die Mappe '%s' tatsächlich löschen?".format(obj.title.get))
  }

  def listDashboard = list

}

abstract class DocumentFoldersLiftScreen extends StructuredLiftScreen[DocumentFolder](DocumentFolders) with FieldTransforms[DocumentFolder] {

  private def userIsRecipient(obj: DocumentFolder): Boolean = {
    (for (user <- User.currentUser) yield {
      obj.userIsRecipient(user, obj.author.get)
    }) openOr false
  }

  private def userIsAuthor(obj: DocumentFolder): Boolean = {
    (for (user <- User.currentUser) yield {
      (obj.author.get == user.id.get) || User.hasPermission(Permissions.DocumentDeleteAll)
    }) openOr false
  }

  private def isDeleteAll(obj: DocumentFolder): Boolean = {
    User.hasPermission(Permissions.DocumentDeleteAll)
  }


  // filter out notes that this user cannot see
  def objViewPermissions(obj: DocumentFolder)   = List(Permissions.DocumentView, userIsAuthor(obj) || userIsRecipient(obj) )
  def objCreatePermissions(obj: DocumentFolder) = Permissions.DocumentCreate :: Nil
  def objEditPermissions(obj: DocumentFolder)   = List(Permissions.DocumentEdit, userIsAuthor(obj) || User.hasPermission(Permissions.DocumentEditAll))
  def objDeletePermissions(obj: DocumentFolder) = List(Permissions.DocumentDelete, userIsAuthor(obj) || User.hasPermission(Permissions.DocumentDeleteAll))

  override def hasUploadField = true

  tfield("Mappe"    , "input-small"      , screenVar.is.folderCreated, ftrans(removeIf(!IsReadOnly)))
  tfield("Mappe"    , "input-xlarge"     , screenVar.is.title)
  tfield("Mappe"    , "input-large"      , screenVar.is.folderIconType)
  tfield("Mappe"    , "input-xlarge"     , screenVar.is.recipients)
  tfield("Mappe"    , ""                 , screenVar.is.oneRecipient, ftrans(removeIf(IsReadOnly && screenVar.is.oneRecipient.valueBox.isEmpty)))
  tfield("Mappe"    , "input-xxlarge"    , screenVar.is.note)
  tfield("Dokumente", "input-sxxlarge"   , screenVar.is.attachments, ftrans(adaptDocTblReadOnly()))

  def createDataHolder(user: User, activeClient: Box[Client]): DocumentFolder =
    activeClient.map(c => DocumentFolder.createInstance(user, c)) openOrThrowException("Need a Client")

}

object DocumentFoldersModalDialog extends DocumentFoldersLiftScreen with ModalLiftScreenSupport[DocumentFolder] {

  val formName = "documentFoldersModal"

  override def dialogTitle = if (IsNewRecord) "Neue Dokumentenmappe" else "Dokumentenmappe bearbeiten"
  override val screenLegend = "Bitte füllen Sie alle Felder, um eine Dokumentenmappe hinzuzufügen"

  override def doFinish(): JsCmd = {
    if (FileUploadInProgress.is) {
      FileUploadInProgress.set(false)
      Noop
    } else {
      super.doFinish
    }
  }

  def uploadDoneCallback(): JsCmd = {
    doAjaxCallback(()=>renderFormCmd)
  }

  def onFinish(data: DocumentFolder): Unit = {
    S.notice("Verarbeitung erfolgreich.")

    // Check for documents marked for removal
    data.removeMarkedDocuments()
    try {
      println("TRY SAVE")
      data.save(true)
      println("DID SAVE")
    } catch {
      case e: Throwable => e.printStackTrace()
    }

    snippetInstance.is.foreach(_.addOrUpdateModelObj(data))

  }

  override def modalScreenCancelled(data: DocumentFolder): JsCmd = {
    if (IsNewRecord) data.delete_! else {
      for (existing <- DocumentFolder.find(data.id.get)) {
        data.attachments.get.filterNot(existing.attachments.get.contains).
          flatMap(Document.find(_)).foreach(doc => {
          Document.documentService.remove(doc.relativeFilePath)
        })
      }
    }
    super.modalScreenCancelled(data)
  }

}

object DocumentFoldersDetailView extends DocumentFoldersLiftScreen {

  val formName = "documentFoldersDetail"
  override val formTemplateName = DEFAULT_DETAIL_VIEW_TPL
  override def isReadOnly(data: DocumentFolder) = true

  def onFinish(data: DocumentFolder): Unit = { }

}

