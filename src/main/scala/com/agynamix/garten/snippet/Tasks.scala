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
import com.agynamix.garten.model.Task
import com.agynamix.garten.lib.Locs._
import org.joda.time.DateTime


object Tasks extends StructuredMetaSnippet[Task](Task) {

  lazy val listMenuParam =
    Menu.param[Task]("OneTask", Loc.LinkText(a => Text(a.id.get.toString)), id => Task.find(id),
        (obj: Task) => obj.id.get.toString) / "task" / * >> Hidden >> RequireLoggedIn >> SbManagement

}

class Tasks extends StructuredFormSnippet[Task](Tasks, TaskModalDialog, TaskDetailView) {

  override def overridenDispatch: DispatchIt = {
    case "listDashboard" => listDashboard
  }

  lazy val listPaginator = new OffsetPaginator(this) {

    implicit val manifest = Manifest.classType[Task](Task.getClass)

    def masterTableFields = List(DbField(Note.noteCreated, true, true, true),
                             DbField(Task.author, true, true, true),
                             DbField(Task.subject, true, true, true),
                             DbField(Task.dueDate, true, true, true))

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
      val dateNow  = new DateTime(user.timezone.isAsJodaTimeZone).toDateMidnight
      val dateThen = dateNow.plusDays(100)
      val tasks = listPaginator.filterDbPaginationResult(Task.findDashboardTasks(user, clientId, dateNow.toDate, dateThen.toDate, 50)).take(20)
      if (tasks.size > 0) {
        "#tbl-model-list" #> tasks.map(task => {
          "@eachModelObj" #> listElement(user, rec=>Noop)(task)
        }) &
        "@task-list-empty" #> ""
      } else {
        "@task-list-table" #> ""
      }
    }).openOr(notExistent)
  }

  def listElement(user: User, selFunc: Task=>JsCmd)(task: Task) = {
    listRecordElements(user, task, selFunc) &
    "@action [onclick]" #> ajaxInvoke(()=>{
      val href = Tasks.listMenuParam.calcHref(task)
      RedirectTo(href)
    }) &
    bindEdit("@edit", task) &
    bindRemove("@remove", task)
  }

  def doBindRemoveAction(obj: Task) = {
    ajaxRemoveAction(obj, "Notiz löschen", s"Möchten Sie die Notiz '${obj.subject.get}' tatsächlich löschen?")
  }

}

abstract class TaskLiftScreen extends StructuredLiftScreen[Task](Tasks) with FieldTransforms[Task] {

  private def userIsRecipient(obj: Task): Boolean = {
    (for (user <- User.currentUser) yield {
      obj.userIsRecipient(user, obj.author.get)
    }) openOr false
  }

  private def userIsAuthor(obj: Task): Boolean = {
    (for (user <- User.currentUser) yield {
      (obj.author.get == user.id.get) || User.hasPermission(Permissions.TaskDeleteAll)
    }) openOr false
  }

  private def isDeleteAll(obj: Task): Boolean = {
    User.hasPermission(Permissions.TaskDeleteAll)
  }


  // filter out notes that this user cannot see
  def objViewPermissions(obj: Task)   = List(Permissions.TaskView, userIsRecipient(obj) )
  def objCreatePermissions(obj: Task) = Permissions.TaskCreate :: Nil
  def objEditPermissions(obj: Task)   = List(Permissions.TaskEdit, userIsAuthor(obj) || User.hasPermission(Permissions.TaskEditAll))
  def objDeletePermissions(obj: Task) = List(Permissions.TaskDelete, userIsAuthor(obj) || User.hasPermission(Permissions.TaskDeleteAll))

  override def hasUploadField = true

  tfield("Aufgabe"  , "input-small"    , screenVar.is.noteCreated, ftrans(removeIf(!IsReadOnly)))
  tfield("Aufgabe"  , "input-xlarge"   , screenVar.is.subject)
  tfield("Aufgabe"  , "input-full-width" , screenVar.is.note, ftrans(fullWidthField, htmlIf(IsReadOnly, screenVar.is.note.calcFieldId)))
  tfield("Termin", "input-xlarge"   , screenVar.is.recipients)
  tfield("Termin", ""               , screenVar.is.oneRecipient, ftrans(removeIf(IsReadOnly && screenVar.is.oneRecipient.valueBox.isEmpty)))
  tfield("Termin", ""               , screenVar.is.dueDate)
  tfield("Termin"                   , screenVar.is.reminder)
  tfield("Anlagen", "input-sxxlarge" , screenVar.is.attachments, ftrans(adaptDocTblReadOnly()))

  def createDataHolder(user: User, activeClient: Box[Client]) =
    activeClient.map(c => Task.createInstance(user, c)) openOrThrowException("Need a Client")

}

object TaskModalDialog extends TaskLiftScreen with ModalLiftScreenSupport[Task] {

  val formName = "taskModal"

  override def dialogTitle = if (IsNewRecord) "Neue Aufgabe" else "Aufgaben bearbeiten"
  override val screenLegend = "Bitte füllen Sie alle Felder, um eine Aufgabe hinzuzufügen"

  override def doFinish(): JsCmd = {
    if (FileUploadInProgress.is) {
      FileUploadInProgress.set(false)
      Noop
    } else {
      super.doFinish
    }
  }

  override def clientInitializeForm(data: Task, formGuid: String, isReadOnly: Boolean): JsCmd = {
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

  def onFinish(data: Task): Unit = {
    println("Reminders onFinish")
    S.notice("Verarbeitung erfolgreich.")

    // Check for documents marked for removal
    data.removeMarkedDocuments()
    data.save(true)

    snippetInstance.is.foreach(_.addOrUpdateModelObj(data))
  }

//  var isExistingRecord = false
//
//  override def beforeCreateModalScreen(data: Task): Unit = {
//    isExistingRecord = Task.find(data.id.is).isDefined
//  }

  override def modalScreenCancelled(data: Task): JsCmd = {
    // Delete files if they have been uploaded
//    println("IS NEW RECORD: "+IsNewRecord.get)
    if (IsNewRecord) data.delete_! else {
      for (existing <- Task.find(data.id.get)) {
        data.attachments.get.filterNot(existing.attachments.get.contains).
        flatMap(Document.find(_)).foreach(doc => {
          Document.documentService.remove(doc.relativeFilePath)
        })
      }
    }
    super.modalScreenCancelled(data)
  }

}

object TaskDetailView extends TaskLiftScreen {

  val formName = "remindersDetail"
  override val formTemplateName = DEFAULT_DETAIL_VIEW_TPL
  override def isReadOnly(data: Task) = true

  def onFinish(data: Task): Unit = { }

}


