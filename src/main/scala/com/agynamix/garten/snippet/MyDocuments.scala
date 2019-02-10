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
import com.agynamix.garten.config.Permissions._
import com.agynamix.garten.config.Permissions
import com.agynamix.garten.model.Address
import com.agynamix.garten.model.Client
import net.liftweb.util.BaseField
import org.bson.types.ObjectId
import com.agynamix.garten.model.Document
import net.liftweb.sitemap.Loc._
import com.agynamix.garten.lib.Locs._


object MyDocuments extends StructuredMetaSnippet[Document](Document) {

  lazy val listMenuParam =
    Menu.param[Document]("OneDocument", Loc.LinkText(a => Text(a.id.get.toString)), id => Document.find(id),
        (obj: Document) => obj.id.get.toString) / "document" / * >> Hidden >> RequireLoggedIn >> SbManagement

}

class MyDocuments() extends StructuredFormSnippet[Document](MyDocuments, DocumentModalDialog, DocumentDetailView) {

  override def overridenDispatch: DispatchIt = {
    case "listDashboard" => listDashboard
  }

  lazy val listPaginator = new OffsetPaginator(this) {

    implicit val manifest = Manifest.classType[Document](Document.getClass)

    def masterTableFields = List(DbField(Document.createdAt, true, true, true),
                             DbField(Document.displayName, true, true, true),
                             DbField(Document.userId, true, true, true))

//                            DbField(Address.city, true, true, true,
//                                   (f: BaseField, rec: Member) => rec.currentAddress.city.asHtml),
  }


  def listElement(user: User, selFunc: Document=>JsCmd)(doc: Document) = {
    listRecordElements(user, doc, selFunc) &
    "@action [onclick]" #> ajaxInvoke(()=>{
      val href = MyDocuments.listMenuParam.calcHref(doc)
      RedirectTo(href)
    }) &
    "@download [href]" #> doc.downloadDocumentUrl() &
    bindRemove("@remove", doc)
  }

  def doBindRemoveAction(obj: Document) = {
    ajaxRemoveAction(obj, "Dokument löschen",
        "Möchten Sie das Dokument '%s' tatsächlich löschen?".format(obj.displayName.get))
  }

  def listDashboard = list

//  override def selChosenItem(multi: Boolean, onSel: Any=>JsCmd)(record: Member): JsCmd = ajaxInvoke{ ()=>
//    val b = toggleElementSelected(record.id.is)
//    println("Selected: "+record.id+", state: "+b)
//    record.selected(b)
//    if (multi) {
//      if (b) {
//        Run("$('#chk-%s').attr('checked', 'checked');".format(record.id.is.toString))
//      } else {
//        Run("$('#chk-%s').removeAttr('checked');".format(record.id.is.toString))
//      }
//    } else {
//      onSel(SelectedElements(selectedElems.toList, removedElems.toList)) &
//      Run("$('.members-items-chooser.modal').modal('hide')")
//    }
//  }


}

abstract class DocumentLiftScreen extends StructuredLiftScreen[Document](MyDocuments) {

  def objViewPermissions(obj: Document)   = Permissions.DocumentView :: Nil
  def objCreatePermissions(obj: Document) = List(false)
  def objEditPermissions(obj: Document)   = Permissions.DocumentEdit :: Nil
  def objDeletePermissions(obj: Document) = Permissions.DocumentDelete :: Nil

  tfield("Standard", "input-xlarge"    , screenVar.is.fileName)

  def createDataHolder(user: User, activeClient: Box[Client]) =
    activeClient.map(c => Document.createInstance(user, c.id.get)) openOrThrowException("Need a Client")

}

object DocumentModalDialog extends DocumentLiftScreen {

  val formName = "documentModal"

  override def dialogTitle = if (IsNewRecord) "Neues Dokument" else "Dokument bearbeiten"
  override val screenLegend = "Bitte füllen Sie alle Felder, um ein Dokument hinzuzufügen"

  def onFinish(data: Document): Unit = {
    S.notice("Verarbeitung erfolgreich.")
    snippetInstance.is.foreach(_.addOrUpdateModelObj(data))
  }

}

object DocumentDetailView extends DocumentLiftScreen {

  val formName = "documentDetail"
  override val formTemplateName = DEFAULT_DETAIL_VIEW_TPL
  override def isReadOnly(data: Document) = true

  def onFinish(data: Document): Unit = {
  }

}


