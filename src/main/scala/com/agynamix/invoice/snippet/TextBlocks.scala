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
import com.agynamix.invoice.model.TextBlock
import net.liftweb.http.GUIDJsExp
import com.agynamix.garten.config.Permissions
import com.agynamix.garten.model.Client
import com.agynamix.garten.snippet.OffsetPaginator


object TextBlocks extends StructuredMetaSnippet(TextBlock) {

}

class TextBlocks extends StructuredFormSnippet(TextBlocks, TextBlockModalDialog, TextBlockDetailView) {

  lazy val listPaginator = new OffsetPaginator(this) {

    implicit val manifest = Manifest.classType[TextBlock](TextBlock.getClass)

    def masterTableFields = List(DbField(TextBlock.identifier, true, true, true),
                                 DbField(TextBlock.text, true, true, true))
  }

  def listElement(user: User, selFunc: TextBlock=>JsCmd)(obj: TextBlock) = {
    listRecordElements(user, obj, selFunc) &
    bindEdit("@edit", obj) &
    bindRemove("@remove", obj)
  }

  def doBindRemoveAction(obj: TextBlock) = {
    ajaxRemoveAction(obj, "Textblock löschen",
        "Möchten Sie den Textblock '%s' (ID: %s) tatsächlich löschen?".format(obj.identifier.get, obj.id.get))
  }

}

abstract class  TextBlockLiftScreen extends StructuredLiftScreen[TextBlock](TextBlocks) {

  def objViewPermissions(obj: TextBlock)   = Permissions.InvoiceView :: Nil
  def objCreatePermissions(obj: TextBlock) = Permissions.InvoiceCreate :: Nil
  def objEditPermissions(obj: TextBlock)   = Permissions.InvoiceEdit :: Nil
  def objDeletePermissions(obj: TextBlock) = Permissions.InvoiceDelete :: Nil

  tfield(TAB_DEFAULT, "input-xlarge"   , screenVar.is.identifier)
  tfield(TAB_DEFAULT, "input-xlarger"  , screenVar.is.text)

  def createDataHolder(user: User, activeClient: Box[Client]) =
    activeClient.map(c => TextBlock.createInstance(user, c)) openOrThrowException("Need a Client")

}

object  TextBlockModalDialog extends TextBlockLiftScreen {

  def formName = "textBlockModal"

  override val dialogTitle = "Neuer Textblock"
  override val screenLegend = "Bitte füllen Sie alle Felder, um einen neuen Textblock anzulegen"

  override def onFinish(data: TextBlock): Unit = {
    S.notice("Neuer Textblock angelegt.")
    snippetInstance.is.foreach(_.addOrUpdateModelObj(data))
  }

}

object TextBlockDetailView extends TextBlockLiftScreen {

  val formName = "textBlockDetail"
  override val formTemplateName = DEFAULT_DETAIL_VIEW_TPL
  override def isReadOnly(data: TextBlock) = true

  def onFinish(data: TextBlock): Unit = {}

}
