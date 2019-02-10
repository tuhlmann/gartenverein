package com.agynamix.garten.snippet

import net.liftweb.http.SHtml._
import net.liftweb.common._
import net.liftweb.util.Helpers._
import net.liftweb.sitemap._
import scala.xml.Text
import net.liftweb.http.S
import com.agynamix.garten.model.User
import net.liftweb.util.CssSel
import java.util.Date
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds._
import com.agynamix.garten.config.Permissions._
import com.agynamix.garten.config.Permissions
import com.agynamix.garten.model.Client
import com.agynamix.garten.api.FileUploadInProgress
import com.agynamix.garten.model.Document
import net.liftweb.sitemap.Loc._
import com.agynamix.garten.lib.Locs._
import com.agynamix.garten.model.Event
import net.liftweb.json.JsonDSL._
import net.liftweb.json._
import net.liftweb.http.js.JE._
import org.joda.time.DateTime
import com.agynamix.garten.lib.util.JsonCallbackAnonFunc
import net.liftweb.http.SHtml
import net.liftweb.http.js.JE
import net.liftweb.http.JsonContext

object Events extends StructuredMetaSnippet[Event](Event) {

  lazy val listMenuParam =
    Menu.param[Event]("OneEvent", Loc.LinkText(a => Text(a.id.get.toString)), id => Event.find(id),
        (obj: Event) => obj.id.get.toString) / "event" / * >> Hidden >> RequireLoggedIn >> SbManagement

}

class Events extends StructuredFormSnippet[Event](Events, EventModalDialog, EventDetailView) {

  override def overridenDispatch: DispatchIt = {
    case "scripts"       => scripts
    case "listDashboard" => listDashboard
  }

  lazy val listPaginator = new DateFieldPaginator(this, Event.startDate.name) {

    implicit val manifest = Manifest.classType[Event](Event.getClass)

    def masterTableFields = List(DbField(Event.startDate, true, true, true),
                             DbField(Event.author, true, true, true),
                             DbField(Event.subject, true, true, true))
  }

// TODO: Show a field that indicates whether documents are attached.
//                            DbField(Address.city, true, true, true,
//                                   (f: BaseField, rec: Member) => rec.currentAddress.city.asHtml),

  // Show max 30 events from today into the future
  def listDashboard: CssSel = {
    (for {
      user <- User.currentUser
      clientId <- user.activeMembership.clientId
    } yield {
      val dateNow  = new DateTime(user.timezone.isAsJodaTimeZone).toDateMidnight
      val dateThen = dateNow.plusDays(100)
      val events = listPaginator.filterDbPaginationResult(Event.findDashboardEvents(user, clientId, dateNow.toDate, dateThen.toDate, 50)).take(20)
      if (events.size > 0) {
        "#tbl-model-list" #> events.map(event => {
          "@eachModelObj" #> listElement(user, rec=>Noop)(event)
        }) &
        "@event-list-empty" #> ""
      } else {
        "@event-list-table" #> ""
      }
    }).openOr(notExistent)
  }

  def listElement(user: User, selFunc: Event=>JsCmd)(appointment: Event) = {
    listRecordElements(user, appointment, selFunc) &
    "@action [onclick]" #> ajaxInvoke(()=>{
      val href = Events.listMenuParam.calcHref(appointment)
      RedirectTo(href)
    }) &
    bindEdit("@edit", appointment) &
    bindRemove("@remove", appointment)
  }

  def doBindRemoveAction(obj: Event) = {
    ajaxRemoveAction(obj, "Notiz löschen", s"Möchten Sie die Notiz '${obj.subject.get}' tatsächlich löschen?")
  }

  override def ajaxRemoveActionJsCmd(obj: Event): JsCmd = {
    super.ajaxRemoveActionJsCmd(obj) &
    Run("App.views.user.Events.removeEvent('%s')".format(obj.id.get.toString))
  }

  def eventsBridge: JsCmd = {
    implicit val formats = DefaultFormats
    // If an exception is thrown during the save, the client automatically gets a Failure
    def doGet(params: JValue): JValue = {
      val re = (for {
        user <- User.currentUser
        clientId <- user.activeMembership.clientId
        start <- (params \ "start").extractOpt[Long]
        startDate <- tryo(new Date(start))
        end   <- (params \ "end").extractOpt[Long]
        endDate <- tryo(new Date(end))
      } yield {
        listPaginator.filterDbPaginationResult(Event.findAppointments(user, clientId, startDate, endDate)).map(myModalScreen.toJValue(this))
      }).openOr(Nil)
      re
    }
    
    // Associate the server functions with client-side functions
//    This is Lift 3 code. Maybe we once again get a chance to use that. 
//    (for (sess <- S.session) yield {
//      val script = Call("App.views.user.Events.setEventsBridge",
//                   sess.buildRoundtrip(List[RoundTripInfo]("get" -> doGet _)))
//
//      script.cmd
//    }) openOr Noop
    
//  Downgrading to Lift 2.6 forced this workaround.
//  Please not that this code is not async, the call blocks until the server is done.
    Call("App.views.user.Events.setEventsBridge",
        AnonFunc("startEndDate, callback", 
            SHtml.jsonCall(JE.JsRaw("startEndDate"), new JsonContext(Full("callback"), Empty), (json: JValue) => doGet(json)).cmd))    
  }

  def createEvent() = {
    implicit val formats = DefaultFormats
    def doCreateEvent(p: JValue) = {
      (for {
        start <- (p \ "start").extractOpt[Long]
        startDate <- tryo(new DateTime(start, User.getDateTimeZone))
        allDay   <- (p \ "allDay").extractOpt[Boolean]
      } yield {
        println("DateTime: "+startDate)
        val dt = startDate.toDate()
        println("Date: "+dt)
        myModalScreen.screenVar.is.startDate(startDate.toDate())
        myModalScreen.screenVar.is.allDay(allDay)
        myModalScreen.createDialog
      }) getOrElse Noop
    }

    if (myModalScreen.isPermittedToCreateElement(myModalScreen.screenVar.is)) {
      Function("appointments_createEvent", List("start", "allDay"), jsonCall(JsRaw("""{'start': start, 'allDay': allDay}"""), (v: JValue) => doCreateEvent(v))._2)
    } else Noop
  }

  def scripts = {
    "*" #> Script(createEvent & eventsBridge)
  }

}

abstract class EventLiftScreen extends StructuredLiftScreen[Event](Events) with FieldTransforms[Event] {

  private def userIsRecipient(obj: Event): Boolean = {
    (for (user <- User.currentUser) yield {
      obj.userIsRecipient(user, obj.author.get)
    }) openOr false
  }

  private def userIsAuthor(obj: Event): Boolean = {
    (for (user <- User.currentUser) yield {
      (obj.author.get == user.id.get) || User.hasPermission(Permissions.EventDeleteAll)
    }) openOr false
  }

  private def isDeleteAll(obj: Event): Boolean = {
    User.hasPermission(Permissions.EventDeleteAll)
  }


  // filter out notes that this user cannot see
  def objViewPermissions(obj: Event)   = List(Permissions.EventView, userIsRecipient(obj) )
  def objCreatePermissions(obj: Event) = Permissions.EventCreate :: Nil
  def objEditPermissions(obj: Event)   = List(Permissions.EventEdit, userIsAuthor(obj) || User.hasPermission(Permissions.EventEditAll))
  def objDeletePermissions(obj: Event) = List(Permissions.EventDelete, userIsAuthor(obj) || User.hasPermission(Permissions.EventDeleteAll))

  override def hasUploadField = true

  tfield("Termin"  , "input-xlarge"     , screenVar.is.subject)
  tfield("Termin"  , ""                 , screenVar.is.startDate, ftrans(hideTimeIfAllDay(screenVar.is.allDay.get), addContainerCls("start-date-input")))
  tfield("Termin"  , ""                 , screenVar.is.endDate, ftrans(hideIf(screenVar.is.allDay.get), addContainerCls("end-date-input")))
  tfield("Termin"  , ""                 , screenVar.is.allDay)
  tfield("Weiteres", "input-xlarge"     , screenVar.is.recipients)
  tfield("Weiteres", ""                 , screenVar.is.oneRecipient, ftrans(removeIf(IsReadOnly && screenVar.is.oneRecipient.valueBox.isEmpty)))
  tfield("Weiteres", ""                 , screenVar.is.reminder)
  tfield("Weiteres", "input-full-width" , screenVar.is.note, ftrans(fullWidthField, htmlIf(IsReadOnly.is, screenVar.is.note.calcFieldId)))
  tfield("Anlagen", "input-sxxlarge"    , screenVar.is.attachments, ftrans(adaptDocTblReadOnly()))


  override def toJValue(snippet: StructuredFormSnippet[Event])(event: Event): JValue = {
    ("title" -> event.subject.get) ~ ("start" -> event.startDate.toUnixSeconds) ~ ("end" -> event.endDate.toUnixSeconds) ~
    ("author" -> event.author.asHtml.toString) ~ ("allDay" -> event.allDay.get) ~ ("desc" -> event.note.asHtml.toString) ~
    ("can_edit" -> isPermittedToEditElement(event)) ~
    ("can_delete" -> isPermittedToDeleteElement(event)) ~
    ("editFunc" -> ajaxInvoke(()=>{
      if (isPermittedToEditElement(event)) setAndRenderModal(event) else Noop
    })._2.toJsCmd.replaceAll("\"", "&quot;")) ~
    ("deleteFunc" -> ajaxInvoke(()=>{
      if (isPermittedToDeleteElement(event)) snippet.doBindRemoveAction(event) else Noop
    })._2.toJsCmd.replaceAll("\"", "&quot;")) ~
    ("id" -> event.id.get.toString)
  }

  def createDataHolder(user: User, activeClient: Box[Client]) =
    activeClient.map(c => Event.createInstance(user, c)) openOrThrowException("Need a Client")

}

object EventModalDialog extends EventLiftScreen with ModalLiftScreenSupport[Event] {

  val formName = "eventsModal"

  override def dialogTitle = if (IsNewRecord) "Neuer Termin" else "Termin bearbeiten"
  override val screenLegend = "Bitte füllen Sie alle Felder, um einen Termin hinzuzufügen"

  override def doFinish(): JsCmd = {
    if (FileUploadInProgress.is) {
      FileUploadInProgress.set(false)
      Noop
    } else {
      super.doFinish
    }
  }

  override def clientInitializeForm(data: Event, formGuid: String, isReadOnly: Boolean): JsCmd = {
    if (!isReadOnly) {
      Run("App.views.common.Common.setupEditorField('%s', true)".format(screenVar.is.note.calcFieldId)) &
      Run("App.views.user.Events.setupEndDateToggle('%s')".format(formGuid))
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

  override def onFinishJsCmd(data: Event) = {
    val js = (for (snippet <- snippetInstance.is) yield {
      Run("App.views.user.Events.addEvent(%s)".format(compact(render(toJValue(snippet)(data)))))
    }) openOr Noop
    val re = super.onFinishJsCmd(data) & js
    //println("JS: "+re)
    re
  }


  def onFinish(data: Event): Unit = {
    println("Reminders onFinish")
    S.notice("Verarbeitung erfolgreich.")

    // Check for documents marked for removal
    data.removeMarkedDocuments()
    data.save(true)

    snippetInstance.is.foreach(_.addOrUpdateModelObj(data))
  }

//  var isExistingRecord = false
//
//  override def beforeCreateModalScreen(data: Event): Unit = {
//    isExistingRecord = Event.find(data.id.is).isDefined
//  }

  override def modalScreenCancelled(data: Event): JsCmd = {
    // Delete files if they have been uploaded
//    println("IS NEW RECORD: "+IsNewRecord.get)
    if (IsNewRecord) data.delete_! else {
      for (existing <- Event.find(data.id.get)) {
        data.attachments.get.filterNot(existing.attachments.get.contains).
        flatMap(Document.find(_)).foreach(doc => {
          Document.documentService.remove(doc.relativeFilePath)
        })
      }
    }
    super.modalScreenCancelled(data)
  }

}

object EventDetailView extends EventLiftScreen {

  val formName = "appointmentsDetail"
  override val formTemplateName = DEFAULT_DETAIL_VIEW_TPL
  override def isReadOnly(data: Event) = true

  def onFinish(data: Event): Unit = { }

}


