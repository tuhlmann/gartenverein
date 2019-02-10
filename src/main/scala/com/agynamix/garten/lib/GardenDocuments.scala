package com.agynamix.garten.lib

import com.agynamix.garten.model.Client
import com.agynamix.garten.model.User
import com.agynamix.garten.model.Membership
import net.liftweb.common._
import net.liftweb.util.Helpers._
import com.agynamix.garten.model.GeneratedDocument
import com.agynamix.garten.model.DocumentTemplate
import com.agynamix.garten.model.DocumentDeliveryOptions
import com.agynamix.garten.model.RecipientList
import com.agynamix.garten.model.RecipientListType
import com.agynamix.garten.model.DocumentType
import net.liftweb.actor.LAFuture
import org.bson.types.ObjectId
import com.agynamix.garten.model.GenerationStatus
import net.liftweb.util.Schedule
import java.text.SimpleDateFormat
import com.agynamix.garten.service.DocumentDownloadService
import com.agynamix.garten.model.Document
import com.agynamix.invoice.model.InvoiceContainer
import com.agynamix.invoice.model.GeneratedInvoice
import net.liftweb.http.S
import com.agynamix.garten.model.Garden

object GardenDocuments {

  val zipFmt = new SimpleDateFormat("yyyyMMdd_HHmm")

  /**
   * - Create 'GeneratedDocument' entry for that user with template for invitation
   * - generate the resulting documents (always 1)
   */
  def createUserInvitationLetter(author: User, client: Client, invitee: User, invitedMember: Membership) {
    for {tpl <- DocumentTemplate.findByIdentifier(client.id.get, DocumentTemplate.StdInvitation.identifier)
         rl <- RecipientList.findSmartList(client.id.get, RecipientListType.Person)
        } yield {
      val subject = "Einladung in den Verein '%s'".format(client.name.get)
      val invitation = GeneratedDocument.createInstance(author, client).templateRef(tpl.id.get).
                        documentType(DocumentType.BulkLetter).
                        delivery(DocumentDeliveryOptions.AllPostalMail).subject(subject).
                        recipients(rl.id.get).oneRecipient(invitedMember.id.get).documentEditable(false).save(true)

      generateDocuments(author, client.id.get, invitation, Full((generator, genDoc, member) => {
        // Check if we need to create a PIN like password and provide this to the invitation letter
        member.userId.obj.flatMap(user => {
          val additionalMapping: Map[String, String] = if (!user.verified.get) {
            val pwd = randomString(8)
            user.password(pwd)
            user.password.hashIt
            user.save(true)
            Map("password" -> pwd)
          } else Map()
          generator.generateForm(genDoc, member, additionalMapping)
        })
      }))

    }
  }

  def generateDocuments(author: User, clientId: ObjectId, document: GeneratedDocument, genFunc: Box[(GardenDocumentGenerator, GeneratedDocument, Membership)=>Box[Document]] = Empty): LAFuture[GeneratedDocument] = {
    val future = new LAFuture[GeneratedDocument]

    (for { recipients <- document.recipients.obj
           generator <- Full(GardenDocumentGenerator(author, clientId, document)) } yield {

      document.generationStatus(GenerationStatus.Processing).save(true)
      Schedule(()=>{
        val generatorFunc: (GardenDocumentGenerator, GeneratedDocument, Membership)=>Box[Document] = genFunc.openOr((generator, genDoc, member) => generator.generateForm(genDoc, member))
        val membersAndDocs = recipients.findRecipientMembers(author.id.get, document).map(m => (m, generatorFunc(generator, document, m)))
        val allDocuments = membersAndDocs.flatMap(_._2)
        val genDate = now
        val allDocName = document.getSubject() + zipFmt.format(genDate) + "_full.zip"
        val allDocDisp = document.getSubject() + " " + zipFmt.format(genDate) + " full"
        val noEmailDocName = document.getSubject() + zipFmt.format(genDate) + "_no_email.zip"
        val noEmailDisp = document.getSubject() + " " + zipFmt.format(genDate) + " no email"
        val zipAllDocs = if (allDocuments.size > 1) DocumentDownloadService.createZipDocument(author, clientId, allDocName, allDocDisp, allDocuments) else Empty
        val noEmailDocs = membersAndDocs.map{ case (member, docBox) =>
          if (member.userEmail.get.isEmpty) docBox else Empty
        }.flatten
        val zipNoEmailDocs = if (noEmailDocs.size > 1) {
          val re = DocumentDownloadService.createZipDocument(author, clientId, noEmailDocName, noEmailDisp, noEmailDocs)
          //println("XXX no emails SIZE: "+noEmailDocs.size+", re: "+re)
          re
        } else {
          Empty
        }
        document.documents(membersAndDocs.flatMap(_._2.map(_.id.get))).
            generationStatus(GenerationStatus.Processed).
            lastGenerated(genDate)
        zipAllDocs.foreach(d => document.zipAllDocuments(d.id.get))
        zipNoEmailDocs.foreach(d => document.zipUsersWithoutEmailDocuments(d.id.get))
        document.save(true)
        println(s"Created ${membersAndDocs.size} documents: "+membersAndDocs.map(i => s"${i._1.userEmail.get}: ${i._2.map(_.documentUrl)}").mkString(", "))
        generator.close()
        future.satisfy(document)
      })
    }) openOr {
      future.abort()
    }
    future
  }


  def generateInvoices(author: User, clientId: ObjectId, invoice: InvoiceContainer, genFunc: Box[(GardenDocumentGenerator, InvoiceContainer, GeneratedInvoice)=>Box[Document]] = Empty): LAFuture[InvoiceContainer] = {
    val future = new LAFuture[InvoiceContainer]

    (for { invoiceTpl <- invoice.templateRef.obj
           generator <- Full(GardenDocumentGenerator(author, clientId, invoiceTpl)) } yield {

      invoice.generationStatus(GenerationStatus.Processing).save(true)
      Schedule(()=>{
        val generatorFunc: (GardenDocumentGenerator, InvoiceContainer, GeneratedInvoice)=>Box[Document] = genFunc.openOr((generator, invoice, genInvoice) => generator.generateInvoice(invoice, genInvoice))
        val membersAndDocs = invoice.generatedInvoices.get.flatMap(genInvoice => {
          for (member <- genInvoice.recipient.obj) yield {
            (member, generatorFunc(generator, invoice, genInvoice))
          }
        })
        val invoiceNo = invoice.generatedInvoices.get.map(_.invoiceNo.get)
        val invoiceNoStr = List(invoiceNo.headOption, invoiceNo.lastOption).flatten.mkString("-")
        val allDocuments = membersAndDocs.flatMap(_._2)
        val genDate = now
        val allDocName = "Rechnung_" + invoiceNoStr + zipFmt.format(genDate) + "_full.zip"
        val allDocDisp = "Rechnung " + invoiceNoStr + " " + zipFmt.format(genDate) + " full"
        val noEmailDocName = "Rechnung_" + invoiceNoStr + zipFmt.format(genDate) + "_no_email.zip"
        val noEmailDisp = "Rechnung " + invoiceNoStr + zipFmt.format(genDate) + " no email"
        val zipAllDocs = if (allDocuments.size > 1) DocumentDownloadService.createZipDocument(author, clientId, allDocName, allDocDisp, allDocuments) else Empty
        val noEmailDocs = membersAndDocs.map{ case (member, docBox) =>
          if (member.userEmail.get.isEmpty) docBox else Empty
        }.flatten
        val zipNoEmailDocs = if (noEmailDocs.size > 1) {
          val re = DocumentDownloadService.createZipDocument(author, clientId, noEmailDocName, noEmailDisp, noEmailDocs)
          //println("XXX no emails SIZE: "+noEmailDocs.size+", re: "+re)
          re
        } else {
          Empty
        }
        invoice.documents(membersAndDocs.flatMap(_._2.map(_.id.get)))
               .generationStatus(GenerationStatus.Processed)
               .lastGenerated(genDate)
        zipAllDocs.foreach(d => invoice.zipAllDocuments(d.id.get))
        zipNoEmailDocs.foreach(d => invoice.zipUsersWithoutEmailDocuments(d.id.get))

        // Foreach generated invoice
        // - Foreach invoice item in generated invoice
        //   - if dynamic item
        //     - find all involved data values (we need to reference them somehow)
        //     - for each referenced value
        //       - find the value, enter the invoice id and flag it as invoiced
        invoice.generatedInvoices.get.foreach(genInvoice => {
          for {garden <- genInvoice.garden.obj
               recipient <- genInvoice.recipient.obj} {
            // check the invoice items within the garden record
            Garden.markValuesInvoiced(invoice, genInvoice, garden)
            // check the invoice items within the recipient record
            Membership.markValuesInvoiced(invoice, genInvoice, recipient)
            garden.save(true)
          }
        })

        // Mark the invoice as completed
        invoice.isCompleted(true)
        invoice.save(true)
        println(s"Created ${membersAndDocs.size} documents: "+membersAndDocs.map(i => s"${i._1.userEmail.get}: ${i._2.map(_.documentUrl)}").mkString(", "))
        generator.close()
        future.satisfy(invoice)
      })
    }) openOr {
      future.abort()
    }
    future
  }

}