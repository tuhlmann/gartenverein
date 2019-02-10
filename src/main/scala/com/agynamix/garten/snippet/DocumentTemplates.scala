package com.agynamix.garten.snippet

import reactive.Observing
import reactive.BufferSignal
import com.agynamix.garten.model.DocumentTemplate
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
import com.agynamix.document.DocumentGenerator
import com.agynamix.garten.lib.MongoDataSource
import com.agynamix.garten.lib.GardenDocumentGenerator



object DocumentTemplates extends StructuredMetaSnippet[DocumentTemplate](DocumentTemplate) {

  lazy val listMenuParam =
    Menu.param[DocumentTemplate]("OneTemplate", Loc.LinkText(a => Text(a.id.get.toString)), id => DocumentTemplate.find(id),
        (obj: DocumentTemplate) => obj.id.get.toString) / "template" / * >> Hidden >> RequireLoggedIn >> SbManagement

}

class DocumentTemplates extends StructuredFormSnippet[DocumentTemplate](DocumentTemplates, DocumentTemplateModalDialog, DocumentTemplateDetailView) {

  lazy val listPaginator = new OffsetPaginator(this) {

    implicit val manifest = Manifest.classType[DocumentTemplate](DocumentTemplate.getClass)

    def masterTableFields = List(DbField(DocumentTemplate.createdAt, true, true, true),
                             DbField(DocumentTemplate.templateType, true, true, true),
                             DbField(DocumentTemplate.name, true, true, true),
                             DbField(DocumentTemplate.author, true, true, true))
  }

  def listElement(user: User, selFunc: DocumentTemplate=>JsCmd)(tpl: DocumentTemplate) = {
    listRecordElements(user, tpl, selFunc) &
    "@action [onclick]" #> ajaxInvoke(()=>{
      val href = DocumentTemplates.listMenuParam.calcHref(tpl)
      RedirectTo(href)
    }) &
    bindEdit("@edit", tpl) &
    bindRemove("@remove", tpl)
  }

  def doBindRemoveAction(obj: DocumentTemplate) = {
    ajaxRemoveAction(obj, "Vorlage löschen", s"Möchten Sie die Vorlage '${obj.name.get}' tatsächlich löschen?")
  }

//  override def clientCreateNew = {
//    "@gen-form [onclick]" #> ajaxInvoke(()=>{
//      (for (member <- Member.findByEmail("tuhlmann@agynamix.de")) yield {
//        val document = GardenDocumentGenerator.generateForm(member)
//        val msg = document.map(d => "Document generated: "+d.getFile.getAbsolutePath()) openOr "Document generation failed!"
//        Alert(msg)
//      }) getOrElse Noop
//    })
//  }


}

abstract class DocumentTemplateLiftScreen extends StructuredLiftScreen[DocumentTemplate](DocumentTemplates) with FieldTransforms[DocumentTemplate] {

  // filter out notes that this user cannot see
  def objViewPermissions(obj: DocumentTemplate)   = List(Permissions.TemplateView)
  def objCreatePermissions(obj: DocumentTemplate) = Permissions.TemplateCreate :: Nil
  def objEditPermissions(obj: DocumentTemplate)   = List(Permissions.TemplateEdit)
  def objDeletePermissions(obj: DocumentTemplate) = List(Permissions.TemplateDelete)

  override def hasUploadField = true

  tfield("Vorlage"  , "input-small"    , screenVar.is.createdAt, ftrans(removeIf(!IsReadOnly)))
  tfield("Vorlage"  , "input-xlarge"   , screenVar.is.name)
  tfield("Vorlage"  , "input-xlarge"   , screenVar.is.templateType)
  tfield("Vorlage"  , "input-sxxlarge" , screenVar.is.note)
  tfield("Vorlage"  , "input-sxxlarge" , screenVar.is.attachments, ftrans(adaptDocTblReadOnly()))

  def createDataHolder(user: User, activeClient: Box[Client]) =
    activeClient.map(c => DocumentTemplate.createInstance(user, c)) openOrThrowException("Need a Client")

}

object DocumentTemplateModalDialog extends DocumentTemplateLiftScreen with ModalLiftScreenSupport[DocumentTemplate] {

  val formName = "documentTemplatesModal"

  override def dialogTitle = if (IsNewRecord) "Neue Dokumentenvorlage" else "Dokumentenvorlage bearbeiten"
  override val screenLegend = "Bitte füllen Sie alle Felder, um eine Vorlage hinzuzufügen"

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

  def onFinish(data: DocumentTemplate): Unit = {
    S.notice("Verarbeitung erfolgreich.")

    // Check for documents marked for removal
    data.removeMarkedDocuments()
    data.version(Int.MaxValue)
    data.save(true)

    snippetInstance.is.foreach(_.addOrUpdateModelObj(data))
  }

  override def modalScreenCancelled(data: DocumentTemplate): JsCmd = {
    // Delete files if they have been uploaded
//    println("IS NEW RECORD: "+IsNewRecord.get)
    if (IsNewRecord) data.delete_! else {
      for (existing <- DocumentTemplate.find(data.id.get)) {
        data.attachments.get.filterNot(existing.attachments.get.contains).
        flatMap(Document.find(_)).foreach(doc => {
          Document.documentService.remove(doc.relativeFilePath)
        })
      }
    }
    super.modalScreenCancelled(data)
  }

}

object DocumentTemplateDetailView extends DocumentTemplateLiftScreen {

  val formName = "documentTemplatesDetail"
  override val formTemplateName = DEFAULT_DETAIL_VIEW_TPL
  override def isReadOnly(data: DocumentTemplate) = true

  def onFinish(data: DocumentTemplate): Unit = { }

}


