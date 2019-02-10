package com.agynamix.garten.lib

import net.liftweb.util.Mailer
import net.liftweb.util.Helpers._
import net.liftweb.util.Mailer.From
import net.liftweb.util.Mailer.Subject
import net.liftweb.util.Mailer.To
import net.liftweb.util.Mailer.PlainMailBodyType
import com.agynamix.garten.config.GardenConfig
import com.agynamix.garten.model.Client
import com.agynamix.garten.model.User
import net.liftweb.common._
import com.agynamix.garten.model.LoginToken
import com.agynamix.garten.model.Note
import com.agynamix.garten.config.App
import com.agynamix.garten.model.Task
import com.agynamix.garten.model.Event
import com.agynamix.garten.model.Membership
import com.agynamix.garten.model.GeneratedDocument
import net.liftweb.util.Mailer.PlusImageHolder
import net.liftweb.util.Mailer.XHTMLPlusImages
import scala.xml.Text
import net.liftweb.http.Templates
import com.agynamix.garten.lib.util.SnippetHelpers
import scala.xml.NodeSeq
import net.liftweb.util.Mailer.XHTMLMailBodyType
import com.agynamix.invoice.model.InvoiceContainer

object MailSender extends SnippetHelpers with Loggable {

  private lazy val fromAddress  = GardenConfig.mailerFromAddress.vend
  private lazy val fromAddrName = GardenConfig.mailerFromAddrName.vend

  private val feedbackRecipients  = To("tuhlmann@agynamix.de") :: To("uwe.ranft@googlemail.com") :: Nil
  private val reqInviteRecipients = feedbackRecipients

  def pickFirst(str: String*): Box[String] = str.filterNot(_.trim().isEmpty()).headOption

  def renderCommonBindings(tpl: NodeSeq) = {
    val footer = Templates(List("templates-hidden", "mail", "mail_footer"))
    "#content" #> tpl andThen
    "#footer-row *" #> footer
  }

  def mailTemplate(name: String): Box[(NodeSeq, NodeSeq)] = {
    for {surround <- Templates(List("templates-hidden", "mail", "mail_template"))
         tpl <- Templates(List("templates-hidden", "mail", name))} yield {
      (surround, tpl)
    }
  }

  // send an email to the user with a link for logging in
  def sendLoginToken(user: User): Unit = {

    val siteName = GardenConfig.siteName.vend
    val token = LoginToken.createForUserId(user.id.get)

    val msgTxt =
      s"""
        |Jemand hat in Ihrem Namen einen Link angefordert, um sein Passwort auf der Webseite ${siteName} zu ändern.
        |
        |Wenn Sie diesen Link nicht angefordert haben, können Sie diese Email einfach ignorieren.
        |Nach 48 Stunden wird der enhaltene Link ungültig werden.
        |
        |Um Ihr Passwort zurückzusetzen, klicken Sie bitte auf den folgenden Link oder kopieren
        |Sie ihn in die Adresszeile Ihres Internetbrowsers.
        |
        |${token.loginUrl}
        |
        |Vielen Dank,
        |${GardenConfig.systemName.vend}
      """.stripMargin

    Mailer.sendMail(From(GardenConfig.systemFancyEmail), Subject(s"${siteName} Passwort Hilfe"), To(user.fancyEmail), PlainMailBodyType(msgTxt))
  }

  def sendFeedback(name: String, email: String, message: String) = {

    val msg = """
      |Nachricht von Nutzer: %s
      |Emailadresse:         %s
      |
      |Mitteilung:
      |===========
      |
      |%s
      """.format(name, email, message).stripMargin

    Mailer.sendMail(From(fromAddress, fromAddrName), Subject("Feedback von " + name), PlainMailBodyType(msg) :: feedbackRecipients: _*)
  }

  def sendRequestInviteMail(name: String, email: String, message: String) = {

    val msg = """
      |Ein Nutzer wünscht Zugang zu "Unser Gartenverein"
      |Nutzer       : %s
      |Emailadresse : %s
      |
      |Mitteilung (optional):
      |===========
      |
      |%s
      """.format(name, email, message).stripMargin

    Mailer.sendMail(From(fromAddress, fromAddrName), Subject(s"Nutzer ${name} bittet um Zugang"), PlainMailBodyType(msg) :: reqInviteRecipients: _*)
  }

  def sendTeamInvitationFromOwner(client: Client, inviter: User, invitee: User, invitation: Membership) = {

    val token = LoginToken.createForTeamInvitation(invitee.id.get, invitation.id.get, true)
    val subject = "Einladung zum Verein "+client.name.get

    for ((surround, tpl) <- mailTemplate("owner_invitation")) {
      for {inviteeAdr <- pickFirst(invitee.email.get)} {

        val transform =
          renderCommonBindings(tpl) andThen
          "@client-name *" #> client.name.get &
          "@inviter-name *" #> inviter.displayName &
          "@url [href]" #> token.invitationUrl &
          "@url *" #> token.invitationUrl &
          (if (invitation.invitationMsg.get.nonEmpty) {
            ".infotext *" #> invitation.invitationMsg.asHtml
          } else {
            ".infotext" #> ""
          })

        Mailer.sendMail(From(fromAddress, fromAddrName), Subject(subject), To(inviteeAdr), XHTMLMailBodyType(transform(surround)))
      }
    }

  }

  def sendTeamInvitationFromAdmin(client: Client, invitee: User, invitation: Membership) = {

    val token = LoginToken.createForTeamInvitation(invitee.id.get, invitation.id.get, true)
    val subject = "Einladung zum Verein "+client.name.get

    for ((surround, tpl) <- mailTemplate("admin_invitation")) {
      for {inviteeAdr <- pickFirst(invitee.email.get)} {

        val transform =
          renderCommonBindings(tpl) andThen
          "@client-name *" #> client.name.get &
          "@invitee-name *" #> invitee.displayName &
          "@url [href]" #> token.invitationUrl &
          "@url *" #> token.invitationUrl

        Mailer.sendMail(From(fromAddress, fromAddrName), Subject(subject), To(inviteeAdr), XHTMLMailBodyType(transform(surround)))
      }
    }

  }

  def sendUserAccountWithClientCreated(client: Client, invitee: User, invitation: Membership) = {

    val token = LoginToken.createForTeamInvitation(invitee.id.get, invitation.id.get, true)

    for (inviteeAdr <- pickFirst(invitee.email.get )) {
      println("sendUserAccountCreated to "+inviteeAdr)
       val msg = s"""
        |Hallo ${invitee.displayName},
        |
        |Für Sie wurde ein Nutzerzugang bei
        |http://www.unser-gartenverein.de angelegt.
        |
        |Sie wurden zur Mitarbeit im Verein "${client.name.get}"
        |eingeladen.
        |
        |Um Ihren Zugang und Ihre Teilnahme zu bestätigen, klicken Sie bitte auf folgenden Link:
        |${token.invitationUrl}
        |
        """.stripMargin

      val subject = "Einladung zum Verein " + client.name.get
      Mailer.sendMail(From(fromAddress, fromAddrName), Subject(subject), To(inviteeAdr), PlainMailBodyType(msg))
    }
  }

  def sendUserAccountCreated(invitee: User) = {

    val token = LoginToken.createForUserId(invitee.id.get)

     val msg = s"""
      |Hallo ${invitee.displayName},
      |Für Sie wurde ein Nutzerzugang bei
      |
      |http://www.unser-gartenverein.de angelegt.
      |
      |Um Ihren Zugang zu nutzen und ein Passwort zu vergeben, klicken Sie bitte auf folgenden Link:
      |${token.loginUrl}
      |
      """.stripMargin

    val subject = "Einladung zu Unser-Gartenverein"
    Mailer.sendMail(From(fromAddress, fromAddrName), Subject(subject), To(invitee.email.get), PlainMailBodyType(msg))

  }

  def sendUserAcceptedInvitation(invitee: User, ms: Membership): Unit = {

    for { client <- ms.clientId.obj } {
      val subject = "Einladung angenommen in Verein "+client.name.get
      for ((surround, tpl) <- mailTemplate("invitation_accepted")) {
        for {clientOwner <- Client.findAllOwners(client.id.get)} {

          val transform =
            renderCommonBindings(tpl) andThen
            "@client-name *" #> client.name.get &
            "@client-owner-name *" #> clientOwner.displayName &
            "@invitee-name *" #> invitee.displayName &
            "@invitee-email *" #> invitee.email.get

          Mailer.sendMail(From(fromAddress, fromAddrName), Subject(subject), To(clientOwner.email.get), XHTMLMailBodyType(transform(surround)))
        }
      }
    }

  }

  def sendGeneratedDocumentNotification(doc: GeneratedDocument, attachDocument: Boolean): Unit = {
    val authorEmail = doc.author.obj.map(_.email.get).openOr("")
    val recipients = GeneratedDocument.findRecipientMembers(doc).filter(_.userEmail.get.isDefined).filterNot(_.userEmail.get exists (_ == authorEmail))

    val subject = s"Neues Dokument: ${doc.subject.get}"

    //println("Subject: "+subject)

    for ((surround, tpl) <- mailTemplate("document_notification")) {
      for {recipient <- recipients
           recipientEmail <- recipient.userEmail.get
           document <- doc.documents.objs.find(_.userId.get == recipient.userId.get)
           documentArr <- document.getBytes } {

        val url = s"${App.hostAndPath}/document/${doc.documents.objs.find(_.userId.get == recipient.userId.get).map(_.id.get).getOrElse("")}"

        val transform =
          renderCommonBindings(tpl) andThen
          "@title *" #> subject &
          "@recipient-name *" #> recipient.displayName &
          "@author-name *" #> doc.author.asString &
          "@document-subject *" #> doc.subject.get &
          "@document-url [href]" #> url &
          "@document-url *" #> url &
          ".infotext *" #> doc.note.get &
          (if (attachDocument) {
            ".f-if-not-attached" #> ""
          } else {
            ".f-if-attached" #> ""
          })

        val body = if (attachDocument) {
          val attachment = PlusImageHolder(document.fileName.get, document.mimeType.get, documentArr, true)
          XHTMLPlusImages(transform(surround), attachment)
        } else {
          XHTMLMailBodyType(transform(surround))
        }

        Mailer.sendMail(From(fromAddress, fromAddrName), Subject(subject), To(recipientEmail), body)
      }
    }

  }

  def sendInvoiceNotification(invoice: InvoiceContainer, attachDocument: Boolean): Unit = {
    val authorEmail = invoice.userId.obj.map(_.email.get).openOr("")
    val recipients = InvoiceContainer.getAllGeneratedInvoiceRecipients(invoice).filter(_.userEmail.get.isDefined).filterNot(_.userEmail.get exists (_ == authorEmail))

    for ((surround, tpl) <- mailTemplate("document_notification")) {
      for {recipient <- recipients
           recipientEmail <- recipient.userEmail.get
           generatedInvoice <- invoice.generatedInvoices.findByRecipient(recipient.id.get)
           document <- invoice.documents.objs.find(_.userId.get == recipient.userId.get)
           documentArr <- document.getBytes } {

        val url = s"${App.hostAndPath}/document/${invoice.documents.objs.find(_.userId.get == recipient.userId.get).map(_.id.get).getOrElse("")}"

        val subject = s"Ihre Rechnung ${generatedInvoice.invoiceNo.get} vom ${invoice.invoiceDate.asText}"

        val transform =
          renderCommonBindings(tpl) andThen
          "@title *" #> subject &
          "@recipient-name *" #> recipient.displayName &
          "@author-name *" #> invoice.userId.asString &
          "@document-subject *" #> subject &
          "@document-url [href]" #> url &
          "@document-url *" #> url &
          ".infotext *" #> "" &
          (if (attachDocument) {
            ".f-if-not-attached" #> ""
          } else {
            ".f-if-attached" #> ""
          })

        val body = if (attachDocument) {
          val attachment = PlusImageHolder(document.fileName.get, document.mimeType.get, documentArr, true)
          XHTMLPlusImages(transform(surround), attachment)
        } else {
          XHTMLMailBodyType(transform(surround))
        }

        Mailer.sendMail(From(fromAddress, fromAddrName), Subject(subject), To(recipientEmail), body)
      }
    }

  }

  def sendNoteReminder(note: Note): Unit = {
    val authorEmail = note.author.obj.map(_.email.get).openOr("")
    val recipients = Note.findMailOnlyRecipients(note).filterNot(_.email == authorEmail)

    //println("Note recipients: "+recipients)

    val subject = s"Neue Notiz: ${note.subject.get}"
    val url = s"${App.hostAndPath}/note/${note.id.get}"

    for ((surround, tpl) <- mailTemplate("things_notification")) {
      for {recipient <- recipients} {

        val transform =
          renderCommonBindings(tpl) andThen
          "@title *" #> subject &
          "@recipient-name *" #> recipient.displayName &
          "@author-name *" #> note.author.asString &
          "@subject *" #> note.subject.get &
          "@item-desc-1" #> "eine neue Notiz" &
          "@item-desc-2" #> "die Notiz" &
          "@url [href]" #> url &
          "@url *" #> url &
          ".infotext *" #> note.note.asHtml

        Mailer.sendMail(From(fromAddress, fromAddrName), Subject(subject), To(recipient.email), XHTMLMailBodyType(transform(surround)))
      }
    }

  }

  def sendTaskReminder(task: Task): Unit = {
    val recipients = Task.findReminderRecipients(task)

    //println("Task recipients: "+recipients)

    val subject = s"Erinnerung: ${task.subject.get}"
    val url = s"${App.hostAndPath}/task/${task.id.get}"

    for ((surround, tpl) <- mailTemplate("things_reminder")) {
      for {recipient <- recipients
           user <- User.find(recipient.userId)} {

        val transform =
          renderCommonBindings(tpl) andThen
          "@title *" #> subject &
          "@recipient-name *" #> recipient.displayName &
          "@author-name *" #> task.author.asString &
          "@subject *" #> task.subject.get &
          "@item-desc" #> "die Aufgabe" &
          "@url [href]" #> url &
          "@url *" #> url &
          "@due" #> task.dueDate.asHtml(user) &
          ".infotext *" #> task.note.asHtml

        logger.info(s"Send Task Reminder ${task.id.get} (${task.subject.get}) to ${recipient.email}")
        Mailer.sendMail(From(fromAddress, fromAddrName), Subject(subject), To(recipient.email), XHTMLMailBodyType(transform(surround)))
      }
    }

  }

  def sendEventReminder(event: Event): Unit = {
    val recipients = Event.findReminderRecipients(event)

    //println("Reminder recipients: "+recipients)

    val subject = s"Erinnerung: ${event.subject.get}"
    val url = s"${App.hostAndPath}/event/${event.id.get}"

    for ((surround, tpl) <- mailTemplate("things_reminder")) {
      for {recipient <- recipients
           user <- User.find(recipient.userId)} {

        val transform =
          renderCommonBindings(tpl) andThen
          "@title *" #> subject &
          "@recipient-name *" #> recipient.displayName &
          "@author-name *" #> event.author.asString &
          "@subject *" #> event.subject.get &
          "@item-desc" #> "den Termin" &
          "@url [href]" #> url &
          "@url *" #> url &
          "@due" #> event.startDate.asHtml(user) &
          ".infotext *" #> event.note.asHtml

        logger.info(s"Send Event Reminder ${event.id.get} (${event.subject.get}) to ${recipient.email}")
        Mailer.sendMail(From(fromAddress, fromAddrName), Subject(subject), To(recipient.email), XHTMLMailBodyType(transform(surround)))
      }
    }

  }


}