package com.agynamix.invoice.lib

import net.liftweb.util.Helpers._
import scala.xml.NodeSeq
import net.liftweb.http.S
import net.liftweb.common._
import com.agynamix.invoice.model.InvoiceItem
import com.agynamix.invoice.model.InvoiceArticle
import com.agynamix.invoice.model.InvoiceArticleType
import org.bson.types.ObjectId
import com.agynamix.invoice.model.InvoiceContainer
import com.agynamix.garten.lib.util.SnippetHelpers
import com.agynamix.invoice.model.InvoiceItemContainer

trait InvoiceItemProcessor {

  def item: InvoiceItem

  def toForm: NodeSeq

}

class ManualArticleInvoiceItemProcessor(val invoice: InvoiceItemContainer[_], val item: InvoiceItem) extends InvoiceItemProcessor {

  def this(invoice: InvoiceItemContainer[_]) = this(invoice, InvoiceItem.createInstance(invoice).articleType(InvoiceArticleType.Manual))

  def formTpl = S.runTemplate(List("templates-hidden", "parts", "form", "invoice", "invoice-manual-article")).openOr(<div>Template not found</div>)

  def toForm: NodeSeq = {

    val css =
      "@units" #> item.units.toForm &
      "@unit-type" #> item.unitType.toForm &
      "@amount" #> item.amount.toForm &
      invoice.clientId.obj.map(client => {
        if (client.deductVat.get) {
          "@vat-ref" #> item.vat.toForm
        } else {
          "@vat-ref-row" #> ""
        }
      }).openOr(SnippetHelpers.notExistent) &
      "@description" #> item.description.toForm

    css(formTpl)

  }
}

class RegularArticleInvoiceItemProcessor(val invoice: InvoiceItemContainer[_], val invoiceArticle: InvoiceArticle, val item: InvoiceItem) extends InvoiceItemProcessor {

  def this(invoice: InvoiceItemContainer[_], invoiceArticle: InvoiceArticle) =
    this(invoice, invoiceArticle, InvoiceItem.createInstance(invoice, invoiceArticle))

  def formTpl = S.runTemplate(List("templates-hidden", "parts", "form", "invoice", "invoice-regular-article")).openOr(<div>Template not found</div>)

  def toForm: NodeSeq = {

    val css =
      "@units" #> item.units.toForm &
      "@unit-type *" #> item.unitType.asHtml &
      "@amount *" #> item.amount.asHtml &
      invoice.clientId.obj.map(client => {
        if (client.deductVat.get) {
          "@vat-ref *" #> item.vat.asHtml
        } else {
          "@vat-ref-row" #> ""
        }
      }).openOr(SnippetHelpers.notExistent) &
      "@description *" #> item.description.asHtml

    css(formTpl)

  }

}


class DynamicInvoiceArticleItemProcessor(val invoice: InvoiceItemContainer[_], val invoiceArticle: InvoiceArticle, val item: InvoiceItem) extends InvoiceItemProcessor {

  def this(invoice: InvoiceItemContainer[_], invoiceArticle: InvoiceArticle) =
    this(invoice, invoiceArticle, InvoiceItem.createInstance(invoice, invoiceArticle))


  def toForm: NodeSeq = {
    <p>{invoiceArticle.description.asHtml}</p>
  }

}

class EmptyInvoiceItemProcessor(invoice: InvoiceItemContainer[_]) extends InvoiceItemProcessor {

  val item = InvoiceItem.createInstance(invoice).articleType(InvoiceArticleType.Manual)

  def toForm: NodeSeq = {
    <p>Not Implemented Yet</p>
  }

}