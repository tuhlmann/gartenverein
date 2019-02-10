package com.agynamix.invoice.snippet
import net.liftweb.http.SHtml._
import net.liftweb.http.js.JsCmds._
import net.liftweb.common._
import net.liftweb.util.Helpers._
import net.liftweb.sitemap._
import net.liftweb.sitemap.Loc._
import reactive._
import reactive.web._
import net.liftweb.http.S
import com.agynamix.garten.model.User
import net.liftweb.http.js.JsCmd
import com.agynamix.invoice.model.Vat
import com.agynamix.garten.snippet.StructuredLiftScreen
import com.agynamix.garten.snippet.StructuredMetaSnippet
import com.agynamix.garten.snippet.StructuredFormSnippet
import com.agynamix.garten.snippet.DbField
import com.agynamix.garten.config.Permissions
import com.agynamix.garten.model.Client
import com.agynamix.garten.snippet.OffsetPaginator


object Vats extends StructuredMetaSnippet(Vat) {

}

class Vats extends StructuredFormSnippet(Vats, VatModalDialog, VatDetailView) {

  lazy val listPaginator = new OffsetPaginator(this) {

    implicit val manifest = Manifest.classType[Vat](Vat.getClass)

    def masterTableFields = List(DbField(Vat.name, true, true, true),
                                 DbField(Vat.vat, true, true, true),
                                 DbField(Vat.valid_from, true, true, true))
  }

  def listElement(user: User, selFunc: Vat=>JsCmd)(obj: Vat) = {
    listRecordElements(user, obj, selFunc) &
    bindEdit("@edit", obj) &
    bindRemove("@remove", obj)
  }

  def doBindRemoveAction(obj: Vat) = {
    ajaxRemoveAction(obj, "MwSt-Satz löschen",
        "Möchten Sie den MwSt-Satz '%s' (gültig ab: %s) tatsächlich löschen?".format(obj.name.get, obj.valid_from.asText))
  }


}

abstract class VatLiftScreen extends StructuredLiftScreen[Vat](Vats) {

  def objViewPermissions(obj: Vat)   = Permissions.InvoiceView :: Nil
  def objCreatePermissions(obj: Vat) = Permissions.InvoiceCreate :: Nil
  def objEditPermissions(obj: Vat)   = List(Permissions.InvoiceEdit, obj.editable.get)
  def objDeletePermissions(obj: Vat) = List(Permissions.InvoiceDelete, obj.editable.get)

  tfield(TAB_DEFAULT, "input-xlarge" , screenVar.is.name)
  tfield(TAB_DEFAULT, "input-mini"   , screenVar.is.vat)
  tfield(TAB_DEFAULT, ""             , screenVar.is.valid_from)

  def createDataHolder(user: User, activeClient: Box[Client]): Vat = {
    activeClient.map(c => Vat.createInstance(c)) openOrThrowException("Need a Client")
  }

}

object VatModalDialog extends VatLiftScreen {

  def formName = "gardenModal"

  override val dialogTitle = "Neuer MwSt-Satz"
  override val screenLegend = "Bitte füllen Sie alle Felder, um einen neuen MwSt-Satz anzulegen"

  override def onFinish(data: Vat): Unit = {
    S.notice("Neuer Garten angelegt.")
    snippetInstance.is.foreach(_.addOrUpdateModelObj(data))
  }

}

object VatDetailView extends VatLiftScreen {

  val formName = "gardenDetail"
  override val formTemplateName = DEFAULT_DETAIL_VIEW_TPL
  override def isReadOnly(data: Vat) = true

  def onFinish(data: Vat): Unit = {}

}
