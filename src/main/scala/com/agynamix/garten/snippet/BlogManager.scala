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
import net.liftmodules.widgets.bootstrap.Modal
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
import net.liftweb.http.js.JE._
import com.agynamix.garten.lib.Locs._
import com.agynamix.garten.model.BlogEntry



object BlogManager extends StructuredMetaSnippet[BlogEntry](BlogEntry) {

  lazy val listMenuParam =
    Menu.param[BlogEntry]("BlogEntry", Loc.LinkText(a => Text(a.subject.get)), id => BlogEntry.findBySnakeSubj(id),
        (obj: BlogEntry) => obj.snakifiedSubject.get) / "blog" / * >> Hidden

}

class BlogManager extends StructuredFormSnippet[BlogEntry](BlogManager, BlogModalDialog, BlogDetailView) {

  lazy val listPaginator = new OffsetPaginator(this) {

    implicit val manifest = Manifest.classType[BlogEntry](BlogEntry.getClass)

    def masterTableFields = List(DbField(BlogEntry.createdAt, true, true, true),
                             DbField(BlogEntry.author, true, true, true),
                             DbField(BlogEntry.subject, true, true, true))

// TODO: Show a field that indicates whether documents are attached.
//                            DbField(Address.city, true, true, true,
//                                   (f: BaseField, rec: Member) => rec.currentAddress.city.asHtml),
  }

  def listElement(user: User, selFunc: BlogEntry=>JsCmd)(entry: BlogEntry) = {
    listRecordElements(user, entry, selFunc) &
    "@action [onclick]" #> ajaxInvoke(()=>{
      val href = BlogManager.listMenuParam.calcHref(entry)
      RedirectTo(href)
    }) &
    bindEdit("@edit", entry) &
    bindRemove("@remove", entry)
  }

  def doBindRemoveAction(obj: BlogEntry) = {
    ajaxRemoveAction(obj, "Eintrag löschen", s"Möchten Sie den Eintrag '${obj.subject.get}' tatsächlich löschen?")
  }

}

abstract class BlogLiftScreen extends StructuredLiftScreen[BlogEntry](BlogManager) with FieldTransforms[BlogEntry] {

  // TODO: Open for all users...
  def objViewPermissions(obj: BlogEntry)   = Permissions.SuperUser :: Nil
  def objCreatePermissions(obj: BlogEntry) = Permissions.SuperUser :: Nil
  def objEditPermissions(obj: BlogEntry)   = Permissions.SuperUser :: Nil
  def objDeletePermissions(obj: BlogEntry) = Permissions.SuperUser :: Nil

  tfield("Blog"   , "input-xlarge"     , screenVar.is.subject)
  tfield("Blog"   , "input-full-width" , screenVar.is.entry, ftrans(fullWidthField, htmlIf(IsReadOnly, screenVar.is.entry.calcFieldId)))
  tfield("Anlagen", "input-sxxlarge"   , screenVar.is.attachments, ftrans(adaptDocTblReadOnly()))

  def createDataHolder(user: User, activeClient: Box[Client]) =
    activeClient.map(c => BlogEntry.createInstance(user, c)) openOrThrowException("Need a Client")

}

object BlogModalDialog extends BlogLiftScreen with ModalLiftScreenSupport[BlogEntry] {

  val formName = "blogModal"

  override def dialogTitle = if (IsNewRecord) "Neue Eintrag" else "Eintrag bearbeiten"
  override val screenLegend = "Bitte füllen Sie alle Felder, um eine Eintrag hinzuzufügen"

  override def clientInitializeForm(data: BlogEntry, formGuid: String, isReadOnly: Boolean): JsCmd = {
    if (!isReadOnly) {
      Run("App.views.common.Common.setupEditorField('%s', false, %s)".
          format(screenVar.is.entry.calcFieldId, AnonFunc("guid", registerUploadedDocument(data)).toJsCmd))
    } else Noop
  }

  def registerUploadedDocument(data: BlogEntry): JsCmd = {
    ajaxCall(JsRaw("guid"), s => {
      data.attachments.addDocument(s)
      Noop
    })
  }

  override def clientRenderAjaxFinishButtonJsCmd(finishId: String): JsCmd = {
    Run("""App.views.common.Common.updateEditorOnSubmit("%s", false, %s)""".
    format(screenVar.is.entry.calcFieldId, AnonFunc(super.clientRenderAjaxFinishButtonJsCmd(finishId)).toJsCmd))
  }

  def uploadDoneCallback(): JsCmd = {
    doAjaxCallback(()=>renderFormCmd)
  }

  def onFinish(data: BlogEntry): Unit = {
    S.notice("Verarbeitung erfolgreich.")

    // Check for documents marked for removal
    data.removeMarkedDocuments()
    data.save(true)

    snippetInstance.is.foreach(_.addOrUpdateModelObj(data))
  }

  // FIXME: GROSS ERROR: var in an object- accessed globally, Remove!!!
  var isExistingRecord = false

  override def beforeCreateModalScreen(data: BlogEntry): Unit = {
    isExistingRecord = BlogEntry.find(data.id.get).isDefined
  }

  override def modalScreenCancelled(data: BlogEntry): JsCmd = {
    // Delete files if they have been uploaded
    if (!isExistingRecord) data.delete_!
    super.modalScreenCancelled(data)
  }


}

object BlogDetailView extends BlogLiftScreen {

  val formName = "blogDetail"
  override val formTemplateName = DEFAULT_DETAIL_VIEW_TPL
  override def isReadOnly(data: BlogEntry) = true

  def onFinish(data: BlogEntry): Unit = { }

}


