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
import com.agynamix.garten.model.Membership
import com.agynamix.garten.lib.GardenDocumentGenerator
import java.io.File
import org.artofsolving.jodconverter.process.ProcessManager
import org.artofsolving.jodconverter.process.PureJavaProcessManager
import org.artofsolving.jodconverter.process.SigarProcessManager
import org.artofsolving.jodconverter.process.PureJavaProcessManager
import org.artofsolving.jodconverter.process.ProcessQuery
import net.liftmodules.widgets.bootstrap.ConfirmDialog
import net.liftmodules.widgets.bootstrap.Bs3ConfirmDialog
import com.agynamix.garten.model.GeneratedDocument
import net.liftweb.http.js.JsCmds
import com.agynamix.garten.model.DocumentType
import net.liftweb.util.Schedule
import com.agynamix.garten.model.GenerationStatus
import com.agynamix.garten.service.DocumentDownloadService
import java.text.SimpleDateFormat
import com.agynamix.garten.lib.GardenDocuments
import com.agynamix.garten.lib.MailSender
import com.agynamix.garten.model.DocumentDeliveryOptions

object GeneratedDocuments extends StructuredMetaSnippet[GeneratedDocument](GeneratedDocument) {

  lazy val listMenuParam =
    Menu.param[GeneratedDocument]("OneGeneratedDocument", Loc.LinkText(a => Text(a.id.get.toString)), id => GeneratedDocument.find(id),
        (obj: GeneratedDocument) => obj.id.get.toString) / "generated_document" / * >> Hidden >> RequireLoggedIn >> SbManagement

}

class GeneratedDocuments extends StructuredFormSnippet[GeneratedDocument](GeneratedDocuments, GeneratedDocumentModalDialog, GeneratedDocumentDetailView) with Loggable {

  override def overridenDispatch: DispatchIt = {
    case "createNewBulkLetter"   => createNewBulkLetter
  }

  lazy val listPaginator = new InMemoryPaginator(this) {

    implicit val manifest = Manifest.classType[GeneratedDocument](GeneratedDocument.getClass)

    def masterTableFields = List(DbField(GeneratedDocument.createdAt, true, true, true),
                                 DbField(GeneratedDocument.author, true, true, true),
                                 DbField(GeneratedDocument.recipients, true, true, true),
                                 DbField(GeneratedDocument.subject, true, true, true)
                             )

/*
                                 DbField(GeneratedDocument.recipients, true, true, true,
                                     (f: BaseField, obj: GeneratedDocument) => obj.recipientsDisplay),

*/
  }


  def listElement(user: User, selFunc: GeneratedDocument=>JsCmd)(doc: GeneratedDocument) = {
    listRecordElements(user, doc, selFunc) &
    withPerm("@letter-preview", modalScreen.objEditPermissions(doc): _*) {
      ajaxInvoke(() =>{
        val expires: Date = millis plus 1.hour
        (for {member <- user.activeMembership.obj.?~("Kein Memberdatensatz gefunden")
              generator <- Full(GardenDocumentGenerator(user, member.clientId.get, doc, expires)) ?~ "Kein Generator erzeugt"
              document <- generator.generateForm(doc, member) ?~ "Kein Dokument generiert" } yield {

          generator.close()
          S.notice("Preview Dokument erzeugt.")
          Run("window.open('%s', '_newtab')".format(document.downloadDocumentUrl()))
        }) openOr {
          S.error("Kein Member Datensatz in Ihrem Nutzer gefunden.")
          Noop
        }
        //val href = BulkLetters.listMenuParam.calcHref(doc)
        //RedirectTo(href)
      })
    } &
    withPerm("@letter-send", modalScreen.objEditPermissions(doc): _*) {
      ajaxInvoke(()=>{
        Bs3ConfirmDialog("Dokument versenden", Text("Bestätigen Sie bitte den sofortigen Versand des Dokumentes"), ()=>{
          (for {member <- user.activeMembership.obj } yield {
            GeneratedDocument.removeReferencedDocuments(doc)
            val future = GardenDocuments.generateDocuments(user, member.clientId.get, doc)
            future.onSuccess(doc => {
              doc.delivery.get match {
                case DocumentDeliveryOptions.PostalAndLink       => MailSender.sendGeneratedDocumentNotification(doc, false)
                case DocumentDeliveryOptions.PostalAndAttachment => MailSender.sendGeneratedDocumentNotification(doc, true)
                case _ =>
              }
              S.notice("Dokumente erstellt.")
            })
            future.onFail(doc => {
              S.notice("Erstellen des Dokumentes fehlgeschlagen.")
            })

            // If documents to download:
            //Run("window.location='%s'".format(document.downloadDocumentUrl()))
            S.notice("Dokumente werden generiert.")
            Noop
          }) openOr {
            S.error("Versand fehlgeschlagen")
            Noop
          }
        })
      })
    } &
    bindEdit("@edit", doc) &
    bindRemove("@remove", doc)
  }

  def doBindRemoveAction(obj: GeneratedDocument) = {
    ajaxRemoveAction(obj, "Dokument löschen", s"Möchten Sie das Dokument '${obj.subject.get}' tatsächlich löschen?")
  }

  override def bindEdit(sel: String, obj: GeneratedDocument): CssSel = {
    val modalScreen = myModalScreen(obj)
    withPerm(sel, modalScreen.objEditPermissions(obj) :_*)(modalScreen.setAndRenderModal(obj))
  }

  override def onSnippetInit(): Unit = {
    BulkLetterModalDialog.snippetInstance(Full(this))
    BulkLetterDetailView.snippetInstance(Full(this))
  }

  override def myModalScreen(data: GeneratedDocument) = data.documentType.get match {
    case DocumentType.BulkLetter => BulkLetterModalDialog
    case _ => GeneratedDocumentModalDialog
  }

  override def myDetailView(data: GeneratedDocument) = data.documentType.get match {
    case DocumentType.BulkLetter => BulkLetterDetailView
    case _ => GeneratedDocumentDetailView
  }


  def createNewBulkLetter = {
    withPerm("@new-bulkletter-screen", BulkLetterModalDialog.objCreatePermissions(BulkLetterModalDialog.screenVar.is) :_*){
      ajaxInvoke(()=> BulkLetterModalDialog.createDialog)
    }
  }

}

abstract class GeneratedDocumentLiftScreen extends StructuredLiftScreen[GeneratedDocument](GeneratedDocuments) with FieldTransforms[GeneratedDocument] {

  // filter out notes that this user cannot see
  def objViewPermissions(obj: GeneratedDocument)   = List(Permissions.GenDocumentView)
  def objCreatePermissions(obj: GeneratedDocument) = Permissions.GenDocumentCreate :: Nil
  def objEditPermissions(obj: GeneratedDocument)   = List(Permissions.GenDocumentEdit, obj.isDocumentEditable)
  def objDeletePermissions(obj: GeneratedDocument) = List(Permissions.GenDocumentDelete)

  tfield("Vorlage"  , "input-small"    , screenVar.is.createdAt, ftrans(removeIf(!IsReadOnly)))
  tfield("Vorlage"  , "input-xlarge"   , screenVar.is.author)
  tfield("Vorlage"  , "input-xlarge"   , screenVar.is.documents)

  def createDataHolder(user: User, activeClient: Box[Client]) =
    activeClient.map(c => GeneratedDocument.createInstance(user, c)) openOrThrowException("Need a Client")

}

object GeneratedDocumentModalDialog extends GeneratedDocumentLiftScreen with ModalLiftScreenSupport[GeneratedDocument] {
  val formName = "generatedDocumentModal"

  def onFinish(data: GeneratedDocument): Unit = {
    S.notice("Verarbeitung erfolgreich.")
    snippetInstance.is.foreach(_.addOrUpdateModelObj(data))
  }
}

object GeneratedDocumentDetailView extends GeneratedDocumentLiftScreen {

  val formName = "generatedDocumentDetail"
  override val formTemplateName = DEFAULT_DETAIL_VIEW_TPL
  override def isReadOnly(data: GeneratedDocument) = true

  def onFinish(data: GeneratedDocument): Unit = { }
}



abstract class BulkLetterLiftScreen extends StructuredLiftScreen[GeneratedDocument](GeneratedDocuments) with FieldTransforms[GeneratedDocument] {

  // filter out notes that this user cannot see
  def objViewPermissions(obj: GeneratedDocument)   = List(Permissions.GenDocumentView)
  def objCreatePermissions(obj: GeneratedDocument) = Permissions.GenDocumentCreate :: Nil
  def objEditPermissions(obj: GeneratedDocument)   = List(Permissions.GenDocumentEdit, obj.isDocumentEditable)
  def objDeletePermissions(obj: GeneratedDocument) = List(Permissions.GenDocumentDelete)

  override def hasUploadField = true

  tfield("Vorlage"  , "input-small"    , screenVar.is.createdAt, ftrans(removeIf(!IsReadOnly)))
  tfield("Vorlage"  , "input-xlarge"   , screenVar.is.subject)
  tfield("Vorlage"  , "input-xlarge"   , screenVar.is.templateRef)
  tfield("Vorlage"  , "input-sxxlarge" , screenVar.is.note)
  tfield("Vorlage"  , "input-sxxlarge" , screenVar.is.attachments, ftrans(adaptDocTblReadOnly()))
  tfield("Versand"  , "input-xlarge"   , screenVar.is.recipients)
  tfield("Versand"  , ""               , screenVar.is.oneRecipient, ftrans(removeIf(IsReadOnly && screenVar.is.oneRecipient.valueBox.isEmpty)))
  tfield("Versand"  , "input-xlarge"   , screenVar.is.delivery)
  tfield("Dokumente", "input-xlarge"   , screenVar.is.generationStatus)
  tfield("Dokumente", "input-xlarge"   , screenVar.is.lastGenerated)
  tfield("Dokumente", "input-xlarge"   , screenVar.is.zipAllDocuments, ftrans(removeIf(screenVar.is.zipAllDocuments.valueBox.isEmpty)))
  tfield("Dokumente", "input-xlarge"   , screenVar.is.zipUsersWithoutEmailDocuments, ftrans(removeIf(screenVar.is.zipUsersWithoutEmailDocuments.valueBox.isEmpty)))
  tfield("Dokumente", "input-sxxlarge" , screenVar.is.documents)

  def createDataHolder(user: User, activeClient: Box[Client]) =
    activeClient.map(c => GeneratedDocument.createInstance(user, c).documentType(DocumentType.BulkLetter)) openOrThrowException("Need a Client")

}

object BulkLetterModalDialog extends BulkLetterLiftScreen with ModalLiftScreenSupport[GeneratedDocument] {

  val formName = "bulkLetterModal"

  override def dialogTitle = if (IsNewRecord) "Neuer Serienbrief" else "Serienbrief bearbeiten"
  override val screenLegend = "Bitte füllen Sie alle Felder, um einen Serienbrief hinzuzufügen"

  override def validations = eitherTemplateOrAttachment _ :: super.validations

  def eitherTemplateOrAttachment(): Errors = {
    if (screenVar.get.templateRef.valueBox.isEmpty && screenVar.get.attachments.get.size == 0) {
      "Bitte entweder eine Vorlage auswählen oder einen Anhang hinzufügen"
    } else Nil
  }

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

  def onFinish(data: GeneratedDocument): Unit = {
    S.notice("Verarbeitung erfolgreich.")

    // Check for documents marked for removal
    data.removeMarkedDocuments()
    data.save(true)

    snippetInstance.is.foreach(_.addOrUpdateModelObj(data))
  }

  override def modalScreenCancelled(data: GeneratedDocument): JsCmd = {
    // Delete files if they have been uploaded
//    println("IS NEW RECORD: "+IsNewRecord.get)
    if (IsNewRecord) data.delete_! else {
      for (existing <- GeneratedDocument.find(data.id.get)) {
        data.attachments.get.filterNot(existing.attachments.get.contains).
        flatMap(Document.find(_)).foreach(doc => {
          Document.documentService.remove(doc.relativeFilePath)
        })
      }
    }
    super.modalScreenCancelled(data)
  }

}

object BulkLetterDetailView extends BulkLetterLiftScreen {

  val formName = "bulkLetterDetail"
  override val formTemplateName = DEFAULT_DETAIL_VIEW_TPL
  override def isReadOnly(data: GeneratedDocument) = true

  def onFinish(data: GeneratedDocument): Unit = { }

}


