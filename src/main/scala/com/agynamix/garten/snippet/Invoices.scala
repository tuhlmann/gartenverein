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
import com.agynamix.document.DocumentGenerator
import com.agynamix.invoice.model.InvoiceContainer
import com.agynamix.garten.model.Garden
import com.agynamix.garten.model.User
import com.agynamix.garten.lib.GardenDocumentGenerator
import net.liftweb.json.JsonAST.JObject
import net.liftweb.mongodb.BsonDSL._
import com.agynamix.garten.config.Permissions
import com.agynamix.garten.model.Client
import com.agynamix.garten.model.DocumentTemplate
import com.agynamix.garten.model.DocumentType
import com.agynamix.garten.config.Permissions._
import net.liftmodules.widgets.bootstrap.Bs3ConfirmDialog
import com.agynamix.garten.lib.GardenDocuments
import com.agynamix.garten.model.DocumentDeliveryOptions
import com.agynamix.garten.lib.MailSender
import com.agynamix.garten.model.GenerationStatus
import com.agynamix.invoice.model.InvoiceFulfillmentType
import com.agynamix.garten.config.Permissions.BoolPerm


object Invoices extends StructuredMetaSnippet[InvoiceContainer](InvoiceContainer) {

  lazy val listMenuParam =
    Menu.param[InvoiceContainer]("OneInvoice", Loc.LinkText(a => Text(a.id.get.toString)), id => InvoiceContainer.find(id),
        (obj: InvoiceContainer) => obj.id.get.toString) / "invoice" / *

}

class Invoices() extends StructuredFormSnippet[InvoiceContainer](Invoices, InvoicesModalDialog, InvoicesDetailView) {

  lazy val listPaginator = new OffsetPaginator(this) {

    implicit val manifest = Manifest.classType[InvoiceContainer](InvoiceContainer.getClass)

    def masterTableFields = List(
                             DbField(InvoiceContainer.invoiceDate, true, true, true),
                             DbField(InvoiceContainer.recipients, true, true, true),
                             DbField(InvoiceContainer.note, false, true, true)
                           )

//    override def keyFields(user: User, args: Any*): Box[()=>JObject] =
//      user.activeMembership.clientId.map(id => ()=>((Invoice.clientId.name -> id) ~ (Invoice.gardenRef.name -> garden.id.is)))
  }

  /**
   * Overrides the keyFields provider to add the current Garden, so the select and count will only consider
   * invoices associated with that Garden.
   */
//  override def keyFields(user: User, args: Any*): (String, List[Any]) =
//    ("%s = ? and %s = ?".format(Invoice.clientId.name, Invoice.gardenRef.name),
//        List(user.activeClientConnection.clientId, garden.id.is))


  def listElement(user: User, selFunc: InvoiceContainer=>JsCmd)(invoice: InvoiceContainer) = {
    listRecordElements(user, invoice, selFunc) &
    "@action [onclick]" #> ajaxInvoke(()=>{
      val href = Invoices.listMenuParam.calcHref(invoice)
      RedirectTo(href)
    }) &
    bindEdit("@edit", invoice) &
    bindRemove("@remove", invoice) &
    withPerm("@invoice-preview", modalScreen.objEditPermissions(invoice): _*) {
      ajaxInvoke(() =>{
        val expires: Date = millis plus 1.hour
        (for {invoiceTpl <- invoice.templateRef.obj
              generator <- Full(GardenDocumentGenerator(user, invoice.clientId.get, invoiceTpl, expires))
              generatedInvoice <- invoice.generatedInvoices.get.headOption
              document <- generator.generateInvoice(invoice, generatedInvoice) } yield {
          generator.close()
          S.notice("Preview Rechnung erzeugt.")
          val re = Run("window.open('%s', '_newtab')".format(document.downloadDocumentUrl()))
          //println("CMD: "+re)
          re
        }) openOr {
          S.error("Rechnung konnte nicht erstellt werden")
          Noop
        }
        //val href = BulkLetters.listMenuParam.calcHref(doc)
        //RedirectTo(href)
      })
    } &
    withPerm("@gen-documents", modalScreen.objEditPermissions(invoice): _*) {
      ajaxInvoke(()=>{
        Bs3ConfirmDialog("Rechnungen erstellen", Text("Möchten Sie die Rechnungen erstellen und an die Empfänger versenden?"), ()=>{
          InvoiceContainer.removeReferencedDocuments(invoice)
          val future = GardenDocuments.generateInvoices(user, invoice.clientId.get, invoice)
          future.onSuccess(invoice => {
            invoice.delivery.get match {
              case DocumentDeliveryOptions.PostalAndLink       => MailSender.sendInvoiceNotification(invoice, false)
              case DocumentDeliveryOptions.PostalAndAttachment => MailSender.sendInvoiceNotification(invoice, true)
              case _ =>
            }
            S.notice("Rechnungen erstellt.")
          })
          future.onFail(doc => {
            S.notice("Erstellen der Rechnungen fehlgeschlagen.")
          })

          // If documents to download:
          //Run("window.location='%s'".format(document.downloadDocumentUrl()))
          S.notice("Rechnungen werden generiert.")
          Noop
        })
      })
    }
    // FIXME: Add an action that only appears for completed invoices
    // to uncomplete them.
  }

  def doBindRemoveAction(obj: InvoiceContainer) = {
    ajaxRemoveAction(obj, "Rechnung löschen",
        "Möchten Sie die Rechnung vom '%s' tatsächlich löschen?".format(obj.invoiceDate.get))
  }

  def listDashboard() = list

//  override def clientCreateNew = {
//    "@member-name *" #> garden.garden_no.get
//  }

//  override def clientCreateNew = {
//    "@gen-report [onclick]" #> ajaxInvoke(()=>{
//      val g = DocumentGenerator()
//      val file = g.generate
//      Alert("Generated file "+file.getAbsolutePath())
//    })
//  }

//  override def saveDataHolder(data: Invoice) = {
//    data.gardenRef(garden.id.is).save
//  }

}

abstract class InvoicesLiftScreen extends StructuredLiftScreen[InvoiceContainer](Invoices) with FieldTransforms[InvoiceContainer] {

  def objViewPermissions(obj: InvoiceContainer)   = Permissions.InvoiceView :: Nil
  def objCreatePermissions(obj: InvoiceContainer) = Permissions.InvoiceCreate :: Nil
  def objEditPermissions(obj: InvoiceContainer)   = List(Permissions.InvoiceEdit, !obj.isCompleted.get)
  def objDeletePermissions(obj: InvoiceContainer) = Permissions.SuperUser :: Nil

  //tfield("Allgemein", "input-small"      , screenVar.is.invoiceNo, true)
  tfield("Allgemein", ""                 , screenVar.is.invoiceDate)
  tfield("Allgemein", "input-xlarge"     , screenVar.is.templateRef)
  tfield("Allgemein", "input-xlarge"     , screenVar.is.recipients)
  tfield("Allgemein", ""                 , screenVar.is.oneRecipient, ftrans(removeIf(IsReadOnly && screenVar.is.oneRecipient.valueBox.isEmpty)))
  tfield("Allgemein", "input-xlarge"     , screenVar.is.delivery)
  tfield("Allgemein", "input-xlarge"     , screenVar.is.fulfillmentType)
  tfield("Allgemein", "input-xlarge"     , screenVar.is.invoiceYear)
  tfield("Allgemein", "input-sxxlarge"   , screenVar.is.note)
  tfield("Rechnung" , "input-margin"     , screenVar.is.invoiceItems, ftrans(fullWidthField))
  tfield("Dokumente", "input-xlarge"     , screenVar.is.generationStatus)
  tfield("Dokumente", "input-xlarge"     , screenVar.is.lastGenerated)
  tfield("Dokumente", "input-xlarge"     , screenVar.is.zipAllDocuments, ftrans(removeIf(screenVar.is.zipAllDocuments.valueBox.isEmpty)))
  tfield("Dokumente", "input-xlarge"     , screenVar.is.zipUsersWithoutEmailDocuments, ftrans(removeIf(screenVar.is.zipUsersWithoutEmailDocuments.valueBox.isEmpty)))
  tfield("Dokumente", "input-sxxlarge"   , screenVar.is.documents)


  override def saveDataHolder(data: InvoiceContainer): Unit =
    for (s <- snippetInstance.is) { s.saveDataHolder(data) }

  def createDataHolder(user: User, activeClient: Box[Client]) =
    activeClient.map(c => InvoiceContainer.createInstance(user, c)) openOrThrowException("Need a Client")

}

object InvoicesModalDialog extends InvoicesLiftScreen {
  val formName = "billingModal"

  override val dialogTitle = if (IsNewRecord) "Neue Rechnung" else "Rechnung bearbeiten"
//  override val screenLegend = ""

  object AddInvoiceItem extends RequestVar(false)

  override def validations = invoiceYearIfYearlyInvoice _ :: super.validations

  def invoiceYearIfYearlyInvoice(): Errors = {
    if (screenVar.get.fulfillmentType.get == InvoiceFulfillmentType.YearlyInvoice &&
        screenVar.get.invoiceYear.valueBox.isEmpty) {
      "Bitte geben Sie das Jahr an, für das die Jahresrechnung erstellt werden soll."
    } else Nil
  }

  override def doFinish(): JsCmd = {
    if (AddInvoiceItem) {
      AddInvoiceItem(false)
      screenVar.is.invoiceItems.addOrEditInvoiceItem
    } else {
      super.doFinish
    }
  }

  def onFinish(data: InvoiceContainer): Unit = {
    S.notice("Verarbeitung erfolgreich.")
    //if (!data.isCompleted.get) {
      println("Create or update invoices")
      InvoiceContainer.createOrUpdateInvoices(data)
    //} else {
    //  println("DO NOT regenerate as it has already been published.")
    //}

    snippetInstance.is.foreach(_.addOrUpdateModelObj(data))
  }

}

object InvoicesDetailView extends InvoicesLiftScreen {

  val formName = "billingDetail"
  override val formTemplateName = DEFAULT_DETAIL_VIEW_TPL
  override def isReadOnly(data: InvoiceContainer) = true

  def onFinish(data: InvoiceContainer): Unit = {
  }

}


