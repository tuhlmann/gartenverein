package com.agynamix.garten.snippet

import reactive.Observing
import reactive.BufferSignal
import com.agynamix.garten.model.Note
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
import com.agynamix.garten.model.User
import net.liftweb.http.PaginatorSnippet
import scala.xml.NodeSeq
import net.liftweb.util.CssSel
import java.util.Date
import net.liftweb.http.FieldBinding
import com.agynamix.garten.lib.field.GenderType
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds._
import com.agynamix.garten.config.Permissions._
import com.agynamix.garten.config.Permissions
import com.agynamix.garten.model.Client
import net.liftweb.util.BaseField
import net.liftweb.http.SHtml
import net.liftweb.http.ScreenFieldInfo
import com.agynamix.garten.api.FileUploadInProgress
import com.agynamix.garten.model.Document
import net.liftweb.sitemap.Loc._
import com.agynamix.garten.lib.Locs._
import net.liftweb.http.js.JE.AnonFunc
import com.agynamix.garten.lib.Locs._
import com.agynamix.garten.lib.MailSender
import org.joda.time.DateTime


object Notes extends StructuredMetaSnippet[Note](Note) {

  lazy val listMenuParam =
    Menu.param[Note]("OneNote", Loc.LinkText(a => Text(a.id.get.toString)), id => Note.find(id),
        (obj: Note) => obj.id.get.toString) / "note" / * >> Hidden >> RequireLoggedIn >> SbManagement

}

class Notes extends StructuredFormSnippet[Note](Notes, NoteModalDialog, NoteDetailView) {

  override def overridenDispatch: DispatchIt = {
    case "listDashboard" => listDashboard
  }

  lazy val listPaginator = new OffsetPaginator(this) {

    implicit val manifest = Manifest.classType[Note](Note.getClass)

    def masterTableFields = List(DbField(Note.noteCreated, true, true, true),
                             DbField(Note.author, true, true, true),
                             DbField(Note.subject, true, true, true),
                             DbField(Note.recipients, true, true, true),
                             DbField(Note.updatedAt, true, true, true))

// TODO: Show a field that indicates whether documents are attached.
//                            DbField(Address.city, true, true, true,
//                                   (f: BaseField, rec: Member) => rec.currentAddress.city.asHtml),
  }

  // Show max 30 events from today into the future
  def listDashboard: CssSel = {
    (for {
      user <- User.currentUser
      clientId <- user.activeMembership.clientId
    } yield {
      val events = listPaginator.filterDbPaginationResult(Note.findNotesOrderByUpdate(user, clientId, 10))
      if (events.size > 0) {
        "#tbl-model-list" #> events.map(event => {
          "@eachModelObj" #> listElement(user, rec=>Noop)(event)
        }) &
        "@note-list-empty" #> ""
      } else {
        "@note-list-table" #> ""
      }
    }).openOr(notExistent)
  }


  def listElement(user: User, selFunc: Note=>JsCmd)(note: Note) = {
    listRecordElements(user, note, selFunc) &
    "@action [onclick]" #> ajaxInvoke(()=>{
      val href = Notes.listMenuParam.calcHref(note)
      RedirectTo(href)
    }) &
    bindEdit("@edit", note) &
    bindRemove("@remove", note)
  }

  def doBindRemoveAction(obj: Note) = {
    ajaxRemoveAction(obj, "Notiz löschen", s"Möchten Sie die Notiz '${obj.subject.get}' tatsächlich löschen?")
  }

}

abstract class NoteLiftScreen extends StructuredLiftScreen[Note](Notes) with FieldTransforms[Note] {

  private def userIsRecipient(obj: Note): Boolean = {
    (for (user <- User.currentUser) yield {
      obj.userIsRecipient(user, obj.author.get)
    }) openOr false
  }

  private def userIsAuthor(obj: Note): Boolean = {
    (for (user <- User.currentUser) yield {
      (obj.author.get == user.id.get) || User.hasPermission(Permissions.NoteDeleteAll)
    }) openOr false
  }

  private def isDeleteAll(obj: Note): Boolean = {
    User.hasPermission(Permissions.NoteDeleteAll)
  }


  // filter out notes that this user cannot see
  def objViewPermissions(obj: Note)   = List(Permissions.NoteView, userIsAuthor(obj) || userIsRecipient(obj) )
  def objCreatePermissions(obj: Note) = Permissions.NoteCreate :: Nil
  def objEditPermissions(obj: Note)   = List(Permissions.NoteEdit, userIsAuthor(obj) || User.hasPermission(Permissions.NoteEditAll))
  def objDeletePermissions(obj: Note) = List(Permissions.NoteDelete, userIsAuthor(obj) || User.hasPermission(Permissions.NoteDeleteAll))

  override def hasUploadField = true

  tfield("Notiz"  , "input-small"      , screenVar.is.noteCreated, ftrans(removeIf(!IsReadOnly)))
  tfield("Notiz"  , "input-xlarge"     , screenVar.is.subject)
  tfield("Notiz"  , "input-xlarge"     , screenVar.is.recipients)
  tfield("Notiz"  , ""                 , screenVar.is.oneRecipient, ftrans(removeIf(IsReadOnly && screenVar.is.oneRecipient.valueBox.isEmpty)))
  tfield("Notiz"  , "input-full-width" , screenVar.is.note, ftrans(fullWidthField, htmlIf(IsReadOnly, screenVar.is.note.calcFieldId)))
  tfield("Anlagen", "input-sxxlarge"   , screenVar.is.attachments, ftrans(adaptDocTblReadOnly()))

  def createDataHolder(user: User, activeClient: Box[Client]) =
    activeClient.map(c => Note.createInstance(user, c)) openOrThrowException("Need a Client")

}

object NoteModalDialog extends NoteLiftScreen with ModalLiftScreenSupport[Note] {

  val formName = "notesModal"

  override def dialogTitle = if (IsNewRecord) "Neue Notiz" else "Notiz bearbeiten"
  override val screenLegend = "Bitte füllen Sie alle Felder, um eine Notiz hinzuzufügen"

  override def doFinish(): JsCmd = {
    if (FileUploadInProgress.is) {
      FileUploadInProgress.set(false)
      Noop
    } else {
      super.doFinish
    }
  }

  override def clientInitializeForm(data: Note, formGuid: String, isReadOnly: Boolean): JsCmd = {
    if (!isReadOnly) {
      Run("App.views.common.Common.setupEditorField('%s', true)".format(screenVar.is.note.calcFieldId))
    } else Noop
  }

  override def clientRenderAjaxFinishButtonJsCmd(finishId: String): JsCmd = {
    Run("""App.views.common.Common.updateEditorOnSubmit("%s", true, %s)""".
    format(screenVar.is.note.calcFieldId, AnonFunc(super.clientRenderAjaxFinishButtonJsCmd(finishId)).toJsCmd))
  }
// , super.clientRenderAjaxFinishButtonJsCmd(finishId)

  def uploadDoneCallback(): JsCmd = {
    doAjaxCallback(()=>renderFormCmd)
  }

  def onFinish(data: Note): Unit = {
    S.notice("Verarbeitung erfolgreich.")

    // Check for documents marked for removal
    data.removeMarkedDocuments()
    data.save(true)

    snippetInstance.is.foreach(_.addOrUpdateModelObj(data))

    if (IsNewRecord) {
      MailSender.sendNoteReminder(data)
    }

  }

//  object isExistingRecord extends RequestVar(false)

//  override def beforeCreateModalScreen(data: Note): Unit = {
//    isExistingRecord.set(Note.find(data.id.get).isDefined)
//    println("IS EXIST SET: "+isExistingRecord)
//  }

  override def modalScreenCancelled(data: Note): JsCmd = {
    // Delete files if they have been uploaded
//    println("IS NEW RECORD: "+IsNewRecord.get)
    if (IsNewRecord) data.delete_! else {
      for (existing <- Note.find(data.id.get)) {
        data.attachments.get.filterNot(existing.attachments.get.contains).
        flatMap(Document.find(_)).foreach(doc => {
          Document.documentService.remove(doc.relativeFilePath)
        })
      }
    }
    super.modalScreenCancelled(data)
  }

}

object NoteDetailView extends NoteLiftScreen {

  val formName = "notesDetail"
  override val formTemplateName = DEFAULT_DETAIL_VIEW_TPL
  override def isReadOnly(data: Note) = true

  def onFinish(data: Note): Unit = { }

}


