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
import com.agynamix.invoice.model.InvoiceArticle
import net.liftweb.http.GUIDJsExp
import com.agynamix.garten.snippet.DbField
import com.agynamix.garten.config.Permissions
import com.agynamix.garten.model.Client
import com.agynamix.garten.snippet.OffsetPaginator
import com.agynamix.invoice.model.InvoiceArticleType
import com.agynamix.garten.config.Permissions._
import com.agynamix.garten.config.Permissions
import net.liftweb.util.CssSel
import com.agynamix.garten.snippet.ModalLiftScreenSupport
import com.agynamix.garten.snippet.FieldTransforms
import net.liftweb.http.RequestVar


object InvoiceArticles extends StructuredMetaSnippet(InvoiceArticle) {

  object DetailViewFormGUID extends RequestVar(nextFuncName)

}

class InvoiceArticles extends StructuredFormSnippet(InvoiceArticles, RegularInvoiceArticleModalDialog, RegularInvoiceArticleDetailView) {

  override def overridenDispatch: DispatchIt = {
    case "createNewInvoiceArticles"   => createNewInvoiceArticles
  }


  lazy val listPaginator = new OffsetPaginator(this) {

    implicit val manifest = Manifest.classType[InvoiceArticle](InvoiceArticle.getClass)

    def masterTableFields = List(DbField(InvoiceArticle.identifier, true, true, true),
                                 DbField(InvoiceArticle.articleType, true, true, true))
  }

  def listElement(user: User, selFunc: InvoiceArticle=>JsCmd)(obj: InvoiceArticle) = {
    listRecordElements(user, obj, selFunc) &
    bindEdit("@edit", obj) &
    bindRemove("@remove", obj)
  }

  def doBindRemoveAction(obj: InvoiceArticle) = {
    ajaxRemoveAction(obj, "Rechnungsartikel löschen",
        "Möchten Sie den Rechnungsartikel '%s' tatsächlich löschen?".format(obj.identifier.get))
  }

  override def onSnippetInit(): Unit = {
    DynamicInvoiceArticleModalDialog.snippetInstance(Full(this))
    DynamicInvoiceArticleDetailView.snippetInstance(Full(this))
  }

  override def myModalScreen(data: InvoiceArticle) = data.articleType.get match {
    case InvoiceArticleType.Dynamic => DynamicInvoiceArticleModalDialog
    case _ => RegularInvoiceArticleModalDialog
  }

  override def myDetailView(data: InvoiceArticle) = {
    data.articleType.get match {
      case InvoiceArticleType.Dynamic => DynamicInvoiceArticleDetailView
      case _ => RegularInvoiceArticleDetailView
    }
  }


  def createNewInvoiceArticles = {
    withPerm("@new-dynamic-article-screen", DynamicInvoiceArticleModalDialog.objCreatePermissions(DynamicInvoiceArticleModalDialog.screenVar.is) :_*){
      ajaxInvoke(()=> DynamicInvoiceArticleModalDialog.createDialog)
    } &
    withPerm("@new-regular-article-screen", RegularInvoiceArticleModalDialog.objCreatePermissions(RegularInvoiceArticleModalDialog.screenVar.is) :_*){
      ajaxInvoke(()=> RegularInvoiceArticleModalDialog.createDialog)
    }
  }

}

abstract class DynamicInvoiceArticleLiftScreen extends StructuredLiftScreen[InvoiceArticle](InvoiceArticles) with FieldTransforms[InvoiceArticle] {

  def objViewPermissions(obj: InvoiceArticle)   = Permissions.InvoiceView :: Nil
  def objCreatePermissions(obj: InvoiceArticle) = Permissions.InvoiceCreate :: Nil
  def objEditPermissions(obj: InvoiceArticle)   = Permissions.InvoiceEdit :: Nil
  def objDeletePermissions(obj: InvoiceArticle) = Permissions.InvoiceDelete :: Nil

  tfield("Allgemein" , "input-sxxlarge" , screenVar.is.identifier)
  tfield("Allgemein" , "input-sxxlarge" , screenVar.is.fulfillmentType)
  tfield("Allgemein" , "input-sxxlarge" , screenVar.is.description)
  tfield("Allgemein" , "input-sxxlarge" , screenVar.is.vat)
  tfield("Berechnung", "input-sxxlarge" , screenVar.is.doForeach)
  tfield("Berechnung", "input-sxxlarge" , screenVar.is.unitFormula)
  tfield("Berechnung", "input-sxxlarge" , screenVar.is.unitType)
  tfield("Berechnung", "input-sxxlarge" , screenVar.is.amountFormula)
  tfield("Berechnung", "input-sxxlarge" , screenVar.is.articleDescription)

  def createDataHolder(user: User, activeClient: Box[Client]) =
    activeClient.map(c => InvoiceArticle.createInstance(user, c).articleType(InvoiceArticleType.Dynamic)) openOrThrowException("Need a Client")

}

object  DynamicInvoiceArticleModalDialog extends DynamicInvoiceArticleLiftScreen with ModalLiftScreenSupport[InvoiceArticle] {

  def formName = "dynamicInvoiceArticleModal"

  override def dialogTitle = if (IsNewRecord) "Neuer dynamischer Artikel" else "Dynamischen Artikel bearbeiten"
  override val screenLegend = "Bitte füllen Sie alle Felder, um einen neuen Rechnungsartikel anzulegen"

  override def onFinish(data: InvoiceArticle): Unit = {
    S.notice("Neuer Rechnungsartikel angelegt.")
    snippetInstance.is.foreach(_.addOrUpdateModelObj(data))
  }

}

object DynamicInvoiceArticleDetailView extends DynamicInvoiceArticleLiftScreen {

  val formName = "dynamicInvoiceArticleDetail"
  override val formTemplateName = DEFAULT_DETAIL_VIEW_TPL
  override def isReadOnly(data: InvoiceArticle) = true

  def onFinish(data: InvoiceArticle): Unit = {}

  override def initCapturedFormGUID = InvoiceArticles.DetailViewFormGUID

}

abstract class RegularInvoiceArticleLiftScreen extends StructuredLiftScreen[InvoiceArticle](InvoiceArticles) with FieldTransforms[InvoiceArticle] {

  def objViewPermissions(obj: InvoiceArticle)   = Permissions.InvoiceView :: Nil
  def objCreatePermissions(obj: InvoiceArticle) = Permissions.InvoiceCreate :: Nil
  def objEditPermissions(obj: InvoiceArticle)   = Permissions.InvoiceEdit :: Nil
  def objDeletePermissions(obj: InvoiceArticle) = Permissions.InvoiceDelete :: Nil

  tfield("Allgemein" , "input-sxxlarge" , screenVar.is.identifier)
  tfield("Allgemein" , "input-sxxlarge" , screenVar.is.fulfillmentType)
  tfield("Allgemein" , "input-sxxlarge" , screenVar.is.description)
  tfield("Allgemein" , "input-sxxlarge" , screenVar.is.vat)
  tfield("Allgemein" , "input-sxxlarge" , screenVar.is.unitType)
  tfield("Allgemein" , "input-sxxlarge" , screenVar.is.amount)

  def createDataHolder(user: User, activeClient: Box[Client]) =
    activeClient.map(c => InvoiceArticle.createInstance(user, c).articleType(InvoiceArticleType.Article)) openOrThrowException("Need a Client")

}

object RegularInvoiceArticleModalDialog extends RegularInvoiceArticleLiftScreen with ModalLiftScreenSupport[InvoiceArticle] {

  def formName = "regularInvoiceArticleModal"

  override def dialogTitle = if (IsNewRecord) "Neuer Stammartikel" else "Stammartikel bearbeiten"
  override val screenLegend = "Bitte füllen Sie alle Felder, um einen neuen Stammartikel anzulegen"

  override def onFinish(data: InvoiceArticle): Unit = {
    S.notice("Neuer Rechnungsartikel angelegt.")
    snippetInstance.is.foreach(_.addOrUpdateModelObj(data))
  }

}

object RegularInvoiceArticleDetailView extends RegularInvoiceArticleLiftScreen {

  val formName = "regularInvoiceArticleDetail"
  override val formTemplateName = DEFAULT_DETAIL_VIEW_TPL
  override def isReadOnly(data: InvoiceArticle) = true

  def onFinish(data: InvoiceArticle): Unit = {}

  override def initCapturedFormGUID = InvoiceArticles.DetailViewFormGUID

}
