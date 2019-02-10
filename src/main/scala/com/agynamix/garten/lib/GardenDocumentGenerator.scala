package com.agynamix.garten.lib

import net.liftweb.common._
import net.liftweb.util.Helpers._
import com.agynamix.document.DocumentHandle
import com.agynamix.invoice.model.InvoiceContainer
import com.agynamix.document.DocumentGenerator
import com.agynamix.garten.model.Membership
import com.agynamix.garten.model.Document
import com.agynamix.document.DataProvider
import com.agynamix.garten.model.User
import com.agynamix.garten.service.DocumentInfo
import com.agynamix.garten.model.Client
import org.bson.types.ObjectId
import com.agynamix.document.OutputDocumentHandle
import java.util.Date
import com.agynamix.garten.model.share.TemplateProvider
import org.artofsolving.jodconverter.process.PureJavaProcessManager
import org.artofsolving.jodconverter.office.DefaultOfficeManagerConfiguration
import org.artofsolving.jodconverter.office.OfficeManager
import org.artofsolving.jodconverter.OfficeDocumentConverter
import java.io.File
import com.agynamix.document.BulkLetterOutputDocumentHandle
import com.agynamix.document.InvoiceDataProvider
import com.agynamix.document.FormLetterDataProvider
import org.apache.commons.io.IOUtils
import java.io.FileInputStream
import java.io.FileOutputStream
import com.agynamix.garten.model.GeneratedDocument
import com.agynamix.invoice.model.GeneratedInvoice
import com.agynamix.document.BasicDataProvider
import com.agynamix.document.DocumentDataProvider

object GardenDocumentGenerator {

  def apply(creator: User, clientId: ObjectId, templateProvider: TemplateProvider, expires: Date = Document.highDate): GardenDocumentGenerator = {
    new GardenDocumentGenerator(creator, clientId, templateProvider, expires)
  }
}

class GardenDocumentGenerator(val creator: User, val clientId: ObjectId,
                              val templateProvider: TemplateProvider, val expires: Date = Document.highDate) extends Loggable {

  private var officeManager: OfficeManager = _
  private var converter : OfficeDocumentConverter = _

  init()

  def init() {
    val processManager = new PureJavaProcessManager()
    officeManager = new DefaultOfficeManagerConfiguration().setProcessManager(processManager).buildOfficeManager()
    officeManager.start();
    converter = new OfficeDocumentConverter(officeManager)
  }

  def close() {
    officeManager.stop()
  }

  def generateInvoice(invoiceContainer: InvoiceContainer, generatedInvoice: GeneratedInvoice, additionalMapping: Map[String, String] = Map()): Box[Document] = {
    generatedInvoice.recipient.obj.flatMap(recipient => {
      val ds = MembershipDataSources(recipient) :::
               (InvoiceContainerDataSource(invoiceContainer, generatedInvoice) ::
               AdditionalMapDataSource(additionalMapping) ::
               OutputDocumentInvoiceDataSource(invoiceContainer, generatedInvoice) :: Nil)
      val dataProvider = new InvoiceDataProvider(templateProvider, ds: _*)
      val userId = recipient.userId.get
      generate(creator, userId, clientId, dataProvider, expires)
    })
  }

  def generateForm(docHolder: GeneratedDocument, member: Membership, additionalMapping: Map[String, String] = Map()): Box[Document] = {
    println("Generate Document for "+member.displayName)
    val ds = MembershipDataSources(member) :::
            (DocumentDataSource(docHolder) :: AdditionalMapDataSource(additionalMapping) :: Nil)
    val dataProvider = new FormLetterDataProvider(templateProvider, ds: _*)
    val userId = member.userId.get
    generate(creator, userId, clientId, dataProvider, expires)
  }

  def generate(creator: User, userId: ObjectId, clientId: ObjectId, dataProvider: DocumentDataProvider, expires: Date): Box[Document] = {
    (for {handle <- DocumentGenerator(dataProvider).generate()
         convertedHandle <- convertToPdf(handle, dataProvider) } yield {
      //val odtName = "/Users/tuhlmann/gen_odt_"+randomString(12)+".odt"
      //IOUtils.copy(new FileInputStream(handle.getFile), new FileOutputStream(odtName))
      handle.getFile.delete()
      createDocumentRecord(creator, userId, clientId, convertedHandle, expires)
    }) openOr Empty
  }

  private def convertToPdf(handle: OutputDocumentHandle, dataProvider: DataProvider): Box[OutputDocumentHandle] = {
    val pdfFile = File.createTempFile("bulk_", ".pdf")
    try {
      converter.convert(handle.getFile, pdfFile)
      Full(new BulkLetterOutputDocumentHandle(pdfFile, templateProvider, dataProvider))
    } catch {
      case e: Exception => e.printStackTrace(); logger.error("Could not convert "+handle.getFile); Empty
    }
  }

  def createDocumentRecord(creator: User, userId: ObjectId, clientId: ObjectId, handle: OutputDocumentHandle, expires: Date): Box[Document] = {
    for { docInfo <- Document.documentService.save(List(clientId.toString(), creator.id.get.toString), handle.getFile, handle.getFileName, handle.getFileDisplayName)} yield {
      val doc = Document.createInstance(userId, creator.id.get, clientId, docInfo).expires(expires).save(true)
      doc
    }
  }

}