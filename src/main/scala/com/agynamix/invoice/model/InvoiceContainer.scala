package com.agynamix.invoice.model


import net.liftweb.common._
import net.liftweb.util.Helpers._
import com.agynamix.garten.model.share.UserId
import com.agynamix.garten.model.share.ClientId
import com.agynamix.garten.lib.StructuredDbObject
import net.liftweb.json.JsonAST.JObject
import java.util.Date
import net.liftweb.mongodb.record.MongoMetaRecord
import com.agynamix.garten.lib.StructuredDbObject
import com.agynamix.garten.model.share.MongoIdRecord
import com.agynamix.garten.model.share.CreatedUpdated
import net.liftweb.mongodb.BsonDSL._
import com.agynamix.garten.model.share.FormValidators
import com.agynamix.garten.model.share.Recipients
import com.agynamix.garten.model.Client
import com.agynamix.garten.model.User
import com.agynamix.garten.lib.field.BsStringField
import com.agynamix.garten.lib.field.BsDateField
import com.agynamix.garten.lib.field.BsTextareaField
import com.mongodb.WriteConcern
import com.agynamix.garten.lib.field.BsObjectIdRefField
import com.agynamix.garten.model.share.OptionalObjectIdRefField
import com.agynamix.garten.model.DocumentTemplate
import org.bson.types.ObjectId
import scala.xml.NodeSeq
import net.liftweb.http.SHtml
import com.agynamix.garten.lib.field.BsEnumNameField
import com.agynamix.garten.model.DocumentDeliveryOptions
import com.agynamix.garten.model.DocumentType
import scala.xml.Text
import net.liftweb.mongodb.record.field.ObjectIdRefField
import net.liftweb.util.FieldError
import com.agynamix.garten.lib.field.BsObjectIdRefListField
import com.agynamix.garten.model.share.GeneratedDocumentsHolder
import net.liftweb.http.S
import net.liftweb.http.S.SFuncHolder
import net.liftweb.mongodb.record.BsonRecord
import net.liftweb.mongodb.record.BsonMetaRecord
import com.agynamix.garten.lib.field.BsIntField
import net.liftweb.mongodb.record.field.BsonRecordListField
import com.agynamix.invoice.lib.ManualArticleInvoiceItemProcessor
import net.liftweb.http.js.JsCmds._
import com.agynamix.invoice.lib.EmptyInvoiceItemProcessor
import com.agynamix.invoice.lib.InvoiceItemProcessor
import com.agynamix.garten.snippet.InvoicesModalDialog
import net.liftweb.http.js.JsCmd
import net.liftweb.mongodb.record.field.ObjectIdPk
import net.liftweb.http.js.jquery.JqJE.JqId
import net.liftweb.http.js.jquery.JqJE.JqRemove
import net.liftweb.mongodb.record.field.ObjectIdField
import com.agynamix.garten.lib.util.LocalizedEnum
import com.agynamix.garten.model.GenerationStatus
import com.agynamix.garten.model.Membership
import com.agynamix.garten.lib.DataSource
import com.agynamix.garten.lib.MongoDataSourceTrait
import com.agynamix.garten.model.Document
import net.liftweb.record.Record
import com.agynamix.invoice.lib.DynamicInvoiceArticleItemProcessor
import com.agynamix.invoice.lib.RegularArticleInvoiceItemProcessor
import com.agynamix.garten.model.Garden
import org.joda.time.DateTime
import com.agynamix.garten.lib.field.OptionalBsIntField
import com.agynamix.garten.lib.field.BsBooleanField

object InvoiceFulfillmentType extends Enumeration with LocalizedEnum {
  type InvoiceFulfillmentType = Value

  def locKeyBase = "invoice.fulfillment.type"

  val Invoice       = Value("invoice")
  val YearlyInvoice = Value("yearly_invoice")
}

trait InvoiceItemContainer[OwnerType <: InvoiceItemContainer[OwnerType]] extends Record[OwnerType] with ClientId[OwnerType] {

  self: OwnerType =>

  /**
   * Needed for communication with the modal dialog.
   */
  def setAddInvoiceItemMarker: Unit

  object invoiceItems extends BsonRecordListField(this, InvoiceItem) {
    override def displayName = "Positionen"
    def formTpl = S.runTemplate(List("templates-hidden", "parts", "form", "invoice", "invoice-items")).openOr(<div>Template not found</div>)

    private var currentArticleType: InvoiceArticleType.Value = InvoiceArticleType.Manual
    private var articlesForType: List[InvoiceArticle] = Nil
    private var currentChosenArticleForType: Box[InvoiceArticle] = Empty
    private var _invoiceItemProcessor: Box[InvoiceItemProcessor] = Empty

    def invoiceItemProcessor = {
      _invoiceItemProcessor openOr {
        val p = createInvoiceItemProcessor(currentArticleType, currentChosenArticleForType)
        _invoiceItemProcessor = Full(p)
        p
      }
    }
    def invoiceItemProcessor_=(proc: InvoiceItemProcessor) = _invoiceItemProcessor = Full(proc)

    val articlesForTypeId = randomString(12)
    val articleAreaId = randomString(12)
    val invoiceItemListId = randomString(12)

    def notCompletedItems = filterNotCompleted(get)

    def filterNotCompleted(list: List[InvoiceItem]): List[InvoiceItem] = {
      list.filterNot(_.isCompleted.get)
    }

    def addOrEditInvoiceItem(): JsCmd = {
      addInvoiceItem(invoiceItemProcessor.item)
      S.notice("Adding Invoice Item")
      invoiceItemProcessor = createInvoiceItemProcessor(currentArticleType, currentChosenArticleForType)
      SetHtml(articleAreaId, invoiceItemProcessor.toForm) &
      SetHtml(invoiceItemListId, renderInvoiceItemList(false))
    }

    def addInvoiceItem(item: InvoiceItem): Unit = {
      val l = if (get.exists(_.id.get == item.id.get)) {
        get.map {
          case it if it.id.get == item.id.get => item
          case it => it
        }
      } else {
        get ::: List(item)
      }

      set(l)
    }

    def removeInvoiceItem(itemId: ObjectId): JsCmd = {
      set(notCompletedItems.filterNot(_.id.get == itemId))
      JqId("item_"+itemId) ~> JqRemove()
    }

    def editInvoiceItem(itemId: ObjectId): JsCmd = {
      (for (item <- notCompletedItems.find(_.id.get == itemId)) yield {
        currentArticleType = item.articleType.get
        articlesForType = findArticlesForType(currentArticleType)
        currentChosenArticleForType = item.referencedArticle.obj
        invoiceItemProcessor = createInvoiceItemProcessor(item, currentChosenArticleForType)
        SetHtml(articlesForTypeId, articlesForTypeSelect(articlesForType)) &
        SetHtml(articleAreaId, invoiceItemProcessor.toForm)
      }) getOrElse Noop
    }

    def createInvoiceItemProcessor(itemType: InvoiceArticleType.Value, invoiceArticle: Box[InvoiceArticle]) = itemType match {
      case InvoiceArticleType.Manual => new ManualArticleInvoiceItemProcessor(owner)
      case InvoiceArticleType.Dynamic if invoiceArticle.isDefined =>
        invoiceArticle.map(a => new DynamicInvoiceArticleItemProcessor(owner, a)) openOr new EmptyInvoiceItemProcessor(owner)
      case InvoiceArticleType.Article if invoiceArticle.isDefined =>
        invoiceArticle.map(a => new RegularArticleInvoiceItemProcessor(owner, a)) openOr new EmptyInvoiceItemProcessor(owner)
      case _  => new EmptyInvoiceItemProcessor(owner)
    }

    def createInvoiceItemProcessor(item: InvoiceItem, invoiceArticle: Box[InvoiceArticle]) = item.articleType.get match {
      case InvoiceArticleType.Manual  => new ManualArticleInvoiceItemProcessor(owner, item)
      case InvoiceArticleType.Dynamic if invoiceArticle.isDefined =>
        invoiceArticle.map(a => new DynamicInvoiceArticleItemProcessor(owner, a, item)) openOr new EmptyInvoiceItemProcessor(owner)
      case InvoiceArticleType.Article if invoiceArticle.isDefined =>
        invoiceArticle.map(a => new RegularArticleInvoiceItemProcessor(owner, a, item)) openOr new EmptyInvoiceItemProcessor(owner)
      case _  => new EmptyInvoiceItemProcessor(owner)
    }

    def renderInvoiceItemList(readOnly: Boolean): NodeSeq = {
      notCompletedItems.zipWithIndex.map{case (item, idx) => {
        <tr id={"item_"+item.id.get}>
          <td>{idx+1}</td>
          <td>{item.description.asShortHtml}</td>
          <td>{item.units.asHtml}</td>
          <td>{item.unitType.asHtml}</td>
          <td>{item.vat.vatAsHtml}</td>
          <td>{item.totalFormatted}</td>
          <td>{if (!readOnly) {
            <div class="pull-right">
            <button class="btn btn-info btn-xs" style="margin-right:5px;"
                    type="button" data-toggle="tooltip"
                    title="Bearbeiten"
                    onclick={SHtml.ajaxInvoke(()=>{editInvoiceItem(item.id.get)})._2.toJsCmd}><i class="icon-edit"></i>
            </button>
            <button class="btn btn-danger btn-xs"
                    type="button" data-toggle="tooltip"
                    title="Löschen"
                    onclick={SHtml.ajaxInvoke(()=>{removeInvoiceItem(item.id.get)})._2.toJsCmd}><i class="icon-remove"></i>
            </button>
            </div>
            } else <span>&nbsp;</span> }
          </td>
        </tr>
      }}
    }

    def articlesForTypeSelect(articlesForType: List[InvoiceArticle]) = {
      if (articlesForType.nonEmpty) {
        val opt = articlesForType.map(a => (a, a.identifier.get))
        SHtml.ajaxSelectObj[InvoiceArticle](opt, currentChosenArticleForType, (s: InvoiceArticle) => {
          currentChosenArticleForType = Full(s)
          invoiceItemProcessor = createInvoiceItemProcessor(s.articleType.get, currentChosenArticleForType)
          SetHtml(articleAreaId, invoiceItemProcessor.toForm)
        }, "class" -> "form-control")
      } else {
        NodeSeq.Empty
      }
    }

    def findArticlesForType(articleType: InvoiceArticleType.Value): List[InvoiceArticle] = {
      InvoiceArticle.findByArticleType(clientId.get, articleType)
    }

    def bindForm(readOnly: Boolean) = {
      val opt = InvoiceArticleType.buildDisplayList(InvoiceArticleType.values.toList).flatMap(r => r._1.map((_, r._2)))

      "@article-type" #> SHtml.ajaxSelectObj[InvoiceArticleType.Value](opt, Full(currentArticleType),
                         (s: InvoiceArticleType.Value)=>{
                           currentArticleType = s
                           articlesForType = findArticlesForType(s)
                           currentChosenArticleForType = articlesForType.headOption
                           invoiceItemProcessor = createInvoiceItemProcessor(s, currentChosenArticleForType)
                           SetHtml(articlesForTypeId, articlesForTypeSelect(articlesForType)) &
                           SetHtml(articleAreaId, invoiceItemProcessor.toForm)
                         }, "class" -> "form-control" ) &
      "@found-articles-for-type [id]" #> articlesForTypeId &
      "@article-input-area [id]" #> articleAreaId &
      "@invoice-item-list [id]" #> invoiceItemListId &
      "@article-add [onclick]" #> SHtml.ajaxInvoke(()=>{
        setAddInvoiceItemMarker
        Noop
      }) &
      "@invoice-item-list *" #> renderInvoiceItemList(readOnly) &
      "@article-input-area *" #> invoiceItemProcessor.toForm
    }


    override def asHtml = {
      val css = {
        "@invoice-item-list *" #> renderInvoiceItemList(true) &
        "@article-input-control-block" #> ""
      }
      <div style="margin-left:10px;margin-right:10px;">{css(formTpl)}</div>
    }

    private def elem = {
      val css = bindForm(false)
      css(formTpl)
    }

    override def toForm: Box[NodeSeq] = Full(elem)

  }


}

object InvoiceContainer extends InvoiceContainer
                        with MongoMetaRecord[InvoiceContainer]
                        with StructuredDbObject[InvoiceContainer]
                        with FormValidators[InvoiceContainer]
                        with Loggable {

  def keyFields(user: User, args: Any*): Box[()=>JObject] =
    user.activeMembership.clientId.map(id => ()=>(clientId.name -> id))

  def createInstance(user: User, client: Client) = InvoiceContainer.createRecord.userId(user.id.get).clientId(client.id.get)

  /**
   * Process the invoice items and generate invoices per recipient and per garden. This can only run if the
   * invoice container is not yet processed.
   */
  def createOrUpdateInvoices(invoice: InvoiceContainer): Unit = {
    if (!invoice.isCompleted.get) {
      logger.debug("in Create or Update Invoices")
      val generatedInvoices = findInvoiceRecipients(invoice, true).flatMap(recipient => {
        val gardens = recipient.gardenRef.objs
        gardens.map(garden => {
          logger.info("For recipient: "+recipient.displayName)
          val existing = invoice.findInvoiceFor(recipient, garden)
          GeneratedInvoice.createInvoice(invoice, recipient, garden, existing)
        })
      })
      invoice.generatedInvoices.set(generatedInvoices)
    } else {
      logger.error("Tried to regenerate invoices but this invoice container is already processed: "+invoice.id.get)
    }
  }

  /**
   * Find those in the list of recipients that are contractors and will receive an invoice
   */
  private def findInvoiceRecipients(invoice: InvoiceContainer, findOnlyNotInvoiced: Boolean): List[Membership] = {
    val all = invoice.recipients.obj.map(_.findRecipientMembers(invoice.userId.get, invoice)).openOr(Nil)
    val filtered = all.filter(_.memberType.isContractor)

    if (findOnlyNotInvoiced && (invoice.fulfillmentType.get == InvoiceFulfillmentType.YearlyInvoice)) {
      filtered.filterNot(r => {
        invoice.invoiceYear.get.map(year => {
          r.yearlyInvoices.notMarkedForYear(year)
        }).getOrElse(false)
      })
    } else filtered
  }


  /**
   * Return a list of recipients from the generated invoices.
   */
  def getAllGeneratedInvoiceRecipients(invoice: InvoiceContainer):List[Membership] = {
    invoice.generatedInvoices.get.flatMap(_.recipient.obj)
  }

  def removeReferencedDocuments(invoice: InvoiceContainer): Unit = {
    Document.removeDocuments(invoice.zipAllDocuments.get :: invoice.zipUsersWithoutEmailDocuments.get :: invoice.documents.get)
  }


  /**
   * Make sure a TeamMember record exists
   * Hook in things that should be checked for new or edited clients
   */
  override def save(invoice: InvoiceContainer, concern: WriteConcern): Boolean = {
    // (Re-)generate contained Invoices, only if not already published
    val re = super.save(invoice, concern)
    //invoice.numberRange.incrementCounter
    re
  }

}

class InvoiceContainer private() extends MongoIdRecord[InvoiceContainer] with UserId[InvoiceContainer] with ClientId[InvoiceContainer]
                        with CreatedUpdated[InvoiceContainer] with Recipients[InvoiceContainer]
                        with GeneratedDocumentsHolder[InvoiceContainer] with MongoDataSourceTrait[InvoiceContainer]
                        with InvoiceItemContainer[InvoiceContainer] {
  def meta = InvoiceContainer

  val datasourceModule = "invoice"

  override def recipientsDisplayname = "Empfänger"

  def setAddInvoiceItemMarker = InvoicesModalDialog.AddInvoiceItem(true)

  object invoiceDate extends BsDateField(this) {
    override def displayName = "Rechnungsdatum"
    override def defaultValue = new Date()
  }

  /**
   * year this invoice is applied to.
   * Valid for Yearly Invoice.
   */
  object invoiceYear extends OptionalBsIntField(this) {
    override def displayName = "Jahr (bei Jahresre.)"
  }

  object numberRange extends BsObjectIdRefField(this, NumberRange) {
    override def defaultValueBox = NumberRange.checkOrInitRange(userId.obj, NumberRangeEnum.Invoices).map(_.id.get)
  }

  object templateRef extends ObjectIdRefField(this, DocumentTemplate) {

    def existingTemplate(msg: => String)(in: ObjectId): List[FieldError] = DocumentTemplate.find(in) match {
      case Full(tpl) => Nil
      case _ => List(FieldError(this, Text(msg)))
    }

    override def displayName = "Vorlage"

    override def validations = existingTemplate("Bitte wählen Sie eine Vorlage") _ :: super.validations

    def templates: List[DocumentTemplate] = DocumentTemplate.findByClient(owner.clientId.get, DocumentType.Invoice)
    override def asHtml = obj.map(_.name.asHtml) openOr Text("")

    override def options: List[(Box[ObjectId], String)] = templates.map(t => (Full(t.id.get), t.name.get))

    override def toForm: Box[NodeSeq] = {
      Full(SHtml.selectObj[Box[ObjectId]](buildDisplayList, Full(obj.map(_.id.get)), v => setBox(v), "class" -> "form-control"))
    }
  }

  object delivery extends BsEnumNameField(this, DocumentDeliveryOptions) {
    override def displayName = "Versand"

    override def buildDisplayList = DocumentDeliveryOptions.buildDisplayList(enum.values.toList)
    override def asHtml = DocumentDeliveryOptions.asHtml(get)
  }

  object fulfillmentType extends BsEnumNameField(this, InvoiceFulfillmentType) {
    override def displayName = "Art der Rechnung"
    override def buildDisplayList = InvoiceFulfillmentType.buildDisplayList(enum.values.toList)
    override def asHtml = InvoiceFulfillmentType.asHtml(get)
    override def helpAsHtml = Full(Text("Art der Rechnung. Bei Auswahl von Jahresrechnung werden die entspr. Jahreskosten der Mitglieder angehängt."))
  }

  /**
   * Marks an InvoiceContainer as completed, invoices have been created and the container
   * must not be regenerated as this would falsify the generated and published invoiced.
   * If the container has to be regenerated, the following steps must be taken:
   * - foreach Membership and Garden, remove the markers that values have been invoiced
   * - remove from Membership.yearlyInvoices, so that this year is open again (if yearly invoice)
   */
  object isCompleted extends BsBooleanField(this) {
    override def defaultValue = false
  }

// These need to go into dynamic invoice positions
//  object invoiceYear extends BsIntField(this) {
//    override def defaultValue = Calendar.getInstance().get(Calendar.YEAR)
//  }

//  object water     extends BsStringField(this, 200) {
//    override def displayName = "Zählerstand Wasser"
//  }
//
//  object electricity extends BsStringField(this, 200) {
//    override def displayName = "Zählerstand Strom"
//  }
//
//  object datumAblesung  extends OptionalDateField(this) with DateFormAdapterField with GermanDateField {
//    override def displayName = "Datum der Ablesung"
//  }

  object note  extends BsTextareaField(this, 1000) {
    override def displayName = "Bemerkung"
    override def textareaRows = 4
  }


  object generatedInvoices extends BsonRecordListField(this, GeneratedInvoice) {

    def findByRecipient(memberId: ObjectId): Box[GeneratedInvoice] = get.find(_.recipient.get == memberId)

  }

  def findInvoiceFor(recipient: Membership, garden: Garden): Box[GeneratedInvoice] = {
    generatedInvoices.get.find(i => (i.recipient.get == recipient.id.get) && (i.garden.get == garden.id.get))
  }

}