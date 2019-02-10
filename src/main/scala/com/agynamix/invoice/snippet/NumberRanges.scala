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
import com.agynamix.invoice.model.NumberRange
import com.agynamix.garten.config.Permissions
import com.agynamix.garten.model.Client
import com.agynamix.garten.snippet.OffsetPaginator


object NumberRanges extends StructuredMetaSnippet(NumberRange) {

}

class NumberRanges extends StructuredFormSnippet(NumberRanges, NumberRangeModalDialog,  NumberRangeDetailView) {

  lazy val listPaginator = new OffsetPaginator(this) {

    implicit val manifest = Manifest.classType[NumberRange](NumberRange.getClass)

    def masterTableFields = List(
                                  DbField(NumberRange.identifier, true, true, true),
                                  DbField(NumberRange.formatStr, true, true, true),
                                  DbField(NumberRange.counter, true, true, true)
                                )
  }

  def listElement(user: User, selFunc: NumberRange=>JsCmd)(obj:  NumberRange) = {
    listRecordElements(user, obj, selFunc) &
    bindEdit("@edit", obj) &
    bindRemove("@remove", obj)
  }

  def doBindRemoveAction(obj: NumberRange) = {
    ajaxRemoveAction(obj, "Nummernbereich löschen",
        "Möchten Sie den Nummernbereich '%s' tatsächlich löschen?".format(obj.identifier.get))
  }

  override def onSnippetInit(): Unit = {
    for (user <- User.currentUser) {
      NumberRange.checkOrInitRanges(user)
    }
  }

}

abstract class  NumberRangeLiftScreen extends StructuredLiftScreen[NumberRange](NumberRanges) {

  def objViewPermissions(obj: NumberRange)   = Permissions.InvoiceView :: Nil
  def objCreatePermissions(obj: NumberRange) = Permissions.InvoiceCreate :: Nil
  def objEditPermissions(obj: NumberRange)   = Permissions.InvoiceEdit :: Nil
  def objDeletePermissions(obj: NumberRange) = Permissions.InvoiceDelete :: Nil

  tfield(TAB_DEFAULT, "input-xlarge" , screenVar.is.identifier)
  tfield(TAB_DEFAULT, "input-small"  , screenVar.is.formatStr)
  tfield(TAB_DEFAULT, "input-mini"   , screenVar.is.counter)

  def createDataHolder(user: User, activeClient: Box[Client]) =
    activeClient.map(c => NumberRange.createInstance(user, c)) openOrThrowException("Need a Client")

}

object  NumberRangeModalDialog extends  NumberRangeLiftScreen {

  def formName = "numberRangeModal"

  override val dialogTitle = "Neuer Nummernkreis"
  override val screenLegend = "Bitte füllen Sie alle Felder, um einen neuen Nummernkreis anzulegen"

  override def onFinish(data: NumberRange): Unit = {
    S.notice("Neuer Nummernkreis angelegt.")
    snippetInstance.is.foreach(_.addOrUpdateModelObj(data))
  }

}

object  NumberRangeDetailView extends  NumberRangeLiftScreen {

  val formName = "numberRangeDetail"
  override val formTemplateName = DEFAULT_DETAIL_VIEW_TPL
  override def isReadOnly(data: NumberRange) = true

  def onFinish(data: NumberRange): Unit = {}

}
