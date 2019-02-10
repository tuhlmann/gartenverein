package com.agynamix.invoice.lib

import com.agynamix.invoice.model.InvoiceContainer
import com.agynamix.garten.model.Membership
import com.agynamix.invoice.model.InvoiceItem
import com.agynamix.invoice.model.InvoiceArticleType
import net.liftweb.common._
import net.liftweb.util.Helpers._
import com.agynamix.invoice.model.InvoiceArticle
import org.joda.money.Money
import org.joda.money.CurrencyUnit
import com.agynamix.garten.lib.DataSource
import com.agynamix.document.DataProvider
import com.agynamix.document.SimpleDataProvider
import com.agynamix.garten.model.Garden
import org.bson.types.ObjectId
import java.util.Date
import com.agynamix.document.MarkInvoicedData
import com.agynamix.document.MarkInvoicedData
import com.agynamix.invoice.model.GeneratedInvoice

object InvoiceItemGenerator {

  /**
   * Can possibly produce multiple InvoiceItems if dynamic article iterates over more than one source (pacht for members for instance)
   */
  def generateInvoiceItems(invoice: InvoiceContainer, recipient: Membership, dataProvider: DataProvider, itemTpl: InvoiceItem, position: Int): List[InvoiceItem] = {
    val items = itemTpl.articleType.get match {
      case InvoiceArticleType.Manual => List(InvoiceItem.createManualArticleCopy(invoice, itemTpl, position))
      case InvoiceArticleType.Article if itemTpl.referencedArticle.obj.isDefined =>
        itemTpl.referencedArticle.obj.map(a => List(InvoiceItem.createCopyWithArticle(invoice, itemTpl, a, position))) openOr Nil
      case InvoiceArticleType.Dynamic if itemTpl.referencedArticle.obj.isDefined =>
        itemTpl.referencedArticle.obj.map(a => generateDynamicInvoiceItems(invoice, recipient, dataProvider, itemTpl, a, position)) openOr Nil
      case _ => Nil
    }

    items
  }

  def generateDynamicInvoiceItems(invoice: InvoiceContainer, recipient: Membership, rootDataProvider: DataProvider, itemTpl: InvoiceItem, article: InvoiceArticle, startPosition: Int): List[InvoiceItem] = {
    println("Generate dynamic")
    val repeaterCmd = article.doForeach.get.trim()
    val repeatedElems = if (repeaterCmd.nonEmpty) {
      rootDataProvider.getRepeatedElems(repeaterCmd)
    } else Nil
    println(s"Repeated Elems for $repeaterCmd: "+repeatedElems)

    val repeatCount = repeatedElems.size max 1
    println("Repeat times = "+repeatCount)
    var position = startPosition
    val items = for (itemCnt <- (1 to repeatCount).toList) yield {
      val dataProvider: DataProvider = repeatedElems.drop(itemCnt-1).headOption match {
        case Some(repeatedElement) =>
          val itsDs = rootDataProvider.dataSources ::: List(repeatedElement)
          new SimpleDataProvider(itsDs: _*)
        case _                     => rootDataProvider
      }

      // We expect the DataProvider now contains everything we need to resolve the mystery of the dynamic article...
      val units: Int    = new UnitParser(dataProvider, article.unitFormula.get).resolve()
      println("Unit: "+units)
      val amount: Money = new AmountParser(dataProvider, article.amountFormula.get).resolve()
      println("Amount: "+amount)
      val description   = new DescriptionParser(dataProvider, article.articleDescription.get).resolve()
      println("Description: "+description)

      val item: InvoiceItem = InvoiceItem.createInstance(invoice).
                  pos(position).
                  articleType(article.articleType.get).
                  units(units).
                  unitType(article.unitType.get).
                  amount(amount).
                  vat(article.vat.get).
                  description(description).
                  referencedArticle(article.id.get)
      position += 1
      println("Created Item: "+item)
      item
    }
    items
  }

  def markValuesInvoiced(invoice: InvoiceContainer, generatedInvoice: GeneratedInvoice, rootDataProvider: DataProvider, itemTpl: InvoiceItem, article: InvoiceArticle): Unit = {
    println("Mark dynamic values as invoiced")
    val repeaterCmd = article.doForeach.get.trim()
    val repeatedElems = if (repeaterCmd.nonEmpty) {
      rootDataProvider.getRepeatedElems(repeaterCmd)
    } else Nil
    println(s"Repeated Elems for $repeaterCmd: "+repeatedElems)

    val repeatCount = repeatedElems.size max 1
    println("Repeat times = "+repeatCount)
    for (itemCnt <- (1 to repeatCount).toList) {
      val dataProvider: DataProvider = repeatedElems.drop(itemCnt-1).headOption match {
        case Some(repeatedElement) =>
          val itsDs = rootDataProvider.dataSources ::: List(repeatedElement)
          new SimpleDataProvider(itsDs: _*)
        case _                     => rootDataProvider
      }

      // We expect the DataProvider now contains everything we need to resolve the mystery of the dynamic article...
      val unitParser = new UnitParser(dataProvider, article.unitFormula.get)
      val amountParser = new AmountParser(dataProvider, article.amountFormula.get)
      unitParser.markValuesInvoiced(MarkInvoicedData(invoice.id.get, generatedInvoice.invoiceNo.get, invoice.invoiceDate.get))
      amountParser.markValuesInvoiced(MarkInvoicedData(invoice.id.get, generatedInvoice.invoiceNo.get, invoice.invoiceDate.get))
    }
  }

  case class FormulaPart(command: String, property: String)
  case class FormulaPartResult(command: String, property: String, value: Any)

  trait FormulaParser extends Loggable {

    val delimLeft  = "\\{\\{"
    val delimRight = "\\}\\}"
    lazy val pattern    = (delimLeft + "([a-zA-Z0-9_:\\-\\.]+)" + delimRight).r


    val formula: String
    val dataProvider: DataProvider

    def hasFormula: Boolean = pattern.findFirstIn(formula).isDefined

    /**
     * Find all enclosed in {{...}}, parse the stuff inside
     */
    def parseFormulaParts(formula: String): List[FormulaPart] = {
      val matches = pattern.findAllMatchIn(formula).toList.flatMap(_.subgroups.headOption)
      println("Matches: "+matches)
      (for (m <- matches) yield {
        val parts = m.split(":").toList
        parts.headOption match {
          case Some("value")  if parts.size == 2 => Full(FormulaPart("value" , parts(1)))
          case _ => logger.error(s"Could not parse formula part: ${m}"); Empty
        }
      }).flatten
    }

    /**
     * resolve the value
     *
     */
    def computeFormulaPart(part: FormulaPart): Box[FormulaPartResult] = {
      val value = dataProvider.getTypedProperty(part.property)
      println(s"Found value ${value.toString()} of type ${value.getClass().getName()}")
      value.map(v => FormulaPartResult(part.command, part.property, v))
    }

    /**
     * FIXME: For now just take the first one - and resolve it.
     */
    private def computeFormula(parts: List[FormulaPart]): Box[FormulaPartResult] = {
      parts.headOption.flatMap(computeFormulaPart)
      // !!! Resolve the property into a real value !!!
    }

    def resolveFormula: Box[FormulaPartResult] = {
      computeFormula(parseFormulaParts(formula))
    }

    def markValuesInvoiced(markInvoicedData: MarkInvoicedData) = {
      for (part <- parseFormulaParts(formula)) {
        dataProvider.markValueInvoiced(part.property, markInvoicedData)
      }

    }

  }

  class UnitParser(val dataProvider: DataProvider, val formula: String) extends FormulaParser {

    def resolve(): Int = {
      if (hasFormula) {
        resolveFormula match {
          case Full(FormulaPartResult(_, _, value: Int))  => value
          case Full(FormulaPartResult(_, _, value: Long)) => value.toInt
          case Full(other) => logger.error(s"Is not an Int in formula ${formula}: ${other}"); 0
          case _ => logger.error(s"Parsing the formula (${formula}) returned nothing"); 0
        }
      } else {
        asInt(formula).openOr(0)
      }
    }
  }

  class AmountParser(val dataProvider: DataProvider, val formula: String) extends FormulaParser {

    lazy val zero = Money.zero(CurrencyUnit.EUR)

    def resolve(): Money = {
      if (hasFormula) {
        resolveFormula match {
          case Full(FormulaPartResult(_, _, value: Money)) => value
          case Full(other) => logger.error(s"Is not a Money value in formula ${formula}: ${other}"); zero
          case _ => logger.error(s"Parsing the formula (${formula}) returned nothing"); zero
        }
      } else {
        asDouble(formula).map(v => Money.of(CurrencyUnit.EUR, v)) openOr zero
      }

    }
  }

  class DescriptionParser(val dataProvider: DataProvider, val formula: String) extends FormulaParser {

    def resolve(): String = {
      val results = parseFormulaParts(formula).map(computeFormulaPart).flatten

      val matches = pattern.findAllIn(formula).toList
      println("Description Matches: "+matches)
      var re = formula
      for (m <- matches) {
        results.find(r => { m.indexOf(r.property) > -1} ).foreach(r => {
          re = re.replace(m, r.value.toString())
        })
      }

      re
    }
  }

}