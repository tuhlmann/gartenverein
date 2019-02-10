package com.agynamix.document

import java.io.InputStream
import java.io.File
import java.io.FileInputStream
import java.io.OutputStream
import java.io.FileOutputStream
import net.liftweb.common._
import com.agynamix.garten.model.share.TemplateProvider
import javax.naming.OperationNotSupportedException
import com.agynamix.garten.lib.DataSource
import org.bson.types.ObjectId
import java.util.Date

case class MarkInvoicedData(invoiceId: ObjectId, generatedInvoiceNo: String, invoiceDate: Date)

trait DataProvider {

  val keyDelim = "\\."

  def getDisplayProperty(key: String): Box[String]
  def getTypedProperty(key: String): Box[Any]

  def getRepeatedElems(key: String): List[DataSource]

  /**
   * A feedback that the value corresponding to the given key has been invoiced.
   * The DataDource may or may not want to log that so that this values will not be processed again.
   */
  def markValueInvoiced(key: String, markInvoicedData: MarkInvoicedData): Unit

  def dataSources: List[DataSource]
}

trait DocumentProvider {

  def documentTemplate: InputDocumentHandle
  def outputDocument: OutputDocumentHandle

}

trait BasicDataProvider extends DataProvider {

  def rootSources: List[DataSource]

  protected def parseKey(key: String): List[String] = key.split(keyDelim).toList

  /**
   * Find a property within this DataProvider
   */
  def getDisplayProperty(key: String): Box[String] = {
    val parsed = parseKey(key)
    rootSources.flatMap(rootDataSource => {
      rootDataSource.findModuleByName(parsed.init).flatMap(_.getDisplayProperty(parsed.last))
    }).headOption
  }

  def getTypedProperty(key: String): Box[Any] = {
    val parsed = parseKey(key)
    rootSources.flatMap(rootDataSource => {
      rootDataSource.findModuleByName(parsed.init).flatMap(_.getTypedProperty(parsed.last))
    }).headOption
  }

  def getRepeatedElems(key: String): List[DataSource] = {
    val parsed = parseKey(key)
    rootSources.flatMap(rootDataSource => {
      rootDataSource.findModuleByName(parsed.init).map(_.getRepeatedElems(parsed.last)).openOr(Nil)
    })
  }

  def markValueInvoiced(key: String, markInvoicedData: MarkInvoicedData): Unit = {
    val parsed = parseKey(key)
    rootSources.foreach(rootDataSource => {
      rootDataSource.findModuleByName(parsed.init).foreach(_.markValueInvoiced(parsed.last, markInvoicedData))
    })
  }


  def dataSources = rootSources

}

trait DocumentDataProvider extends BasicDataProvider with DocumentProvider {

}

class SimpleDataProvider(_rootSources: DataSource*) extends BasicDataProvider {

  val rootSources = _rootSources.toList

}


class InvoiceDataProvider(val tplProvider: TemplateProvider, _rootSources: DataSource*) extends DocumentDataProvider with Loggable {

  val template = tplProvider.templateUrl
  val rootSources = _rootSources.toList

  def documentTemplate = template match {
    case Full(tpl) => new UrlInputDocumentHandle(tpl)
    case _ =>         logger.error("ERROR. NO TEMPLATE FILE!"); new NullInputDocumentHandle()
  }

  lazy val outputDocument = {
    val re = new BulkLetterOutputDocumentHandle(File.createTempFile("gen_invoice", ".odt"), tplProvider, this)
    //println("TMP Document: "+re.getFile)
    re
  }

}

class FormLetterDataProvider(val tplProvider: TemplateProvider, _rootSources: DataSource*) extends DocumentDataProvider with Loggable {

  val template = tplProvider.templateUrl
  val rootSources = _rootSources.toList

  def documentTemplate = template match {
    case Full(tpl) => new UrlInputDocumentHandle(tpl)
    case _ =>         logger.error("ERROR. NO TEMPLATE FILE!"); new NullInputDocumentHandle()
  }

  lazy val outputDocument = {
    val re = new BulkLetterOutputDocumentHandle(File.createTempFile("bulk_letter", ".odt"), tplProvider, this)
    //println("TMP Document: "+re.getFile)
    re
  }

}

class EmptyDataProvider extends DataProvider {

  def documentTemplate: InputDocumentHandle = {
    throw new OperationNotSupportedException("Not Supported")
  }

  def outputDocument: OutputDocumentHandle = {
    throw new OperationNotSupportedException("Not Supported")
  }

  def getDisplayProperty(key: String): Box[String] = Empty
  def getTypedProperty(key: String): Box[Any] = Empty
  def getRepeatedElems(key: String): List[DataSource] = Nil

  def dataSources = Nil

  def markValueInvoiced(key: String, markInvoicedData: MarkInvoicedData): Unit = {}

}

class RepeaterDataProvider(val _rootSources: DataSource*) extends DocumentDataProvider with Loggable {

  val rootSources = _rootSources.toList

  def documentTemplate: InputDocumentHandle = {
    throw new OperationNotSupportedException("Not Supported")
  }

  def outputDocument: OutputDocumentHandle = {
    throw new OperationNotSupportedException("Not Supported")
  }

}

