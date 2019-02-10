package com.agynamix.garten.config

import net.liftweb.common._
import net.liftweb.http.Factory
import com.agynamix.garten.model.User
import com.agynamix.garten.snippet.Sidebar
import org.joda.time.ReadablePeriod
import org.joda.time.Days
import com.agynamix.garten.lib.util.SnippetHelpers
import net.liftweb.http.SessionVar
import net.liftweb.util.Helpers
import org.joda.time.Hours
import com.agynamix.garten.lib.AuthUserMeta
import net.liftweb.util.Props
import com.agynamix.garten.service.DocumentService
import com.agynamix.garten.service.FileDocumentService


object GardenConfig extends Factory {
  // AuthUserMeta object
  val authUserMeta = new FactoryMaker[AuthUserMeta[_]](User) {}

  // urls
  val indexUrl       = new FactoryMaker[String]("/") {}
  val loginUrl       = new FactoryMaker[String]("/login") {}
  val logoutUrl      = new FactoryMaker[String]("/logout") {}
  val registerUrl    = new FactoryMaker[String]("/register") {}
  val setPasswordUrl = new FactoryMaker[String]("/set_password") {}

  // site settings
  val siteName = new FactoryMaker[String]("Example") {}
  val systemEmail = new FactoryMaker[String]("info@example.com") {}
  val systemName = new FactoryMaker[String]("Example Staff") {}

  def systemFancyEmail = AuthUtil.fancyEmail(systemName.vend, systemEmail.vend)

  // LoginToken
  val loginTokenUrl = new FactoryMaker[String]("/login-token") {}
  val loginTokenAfterUrl = new FactoryMaker[String]("/set-password") {}
  val loginTokenExpires = new FactoryMaker[ReadablePeriod](Hours.hours(48)) {}

  // InvitationToken
  val invitationTokenUrl      = new FactoryMaker[String]("/invitation-token") {}
  val invitationTokenAfterUrl = new FactoryMaker[String]("/register") {}
  val invitationTokenExpires  = new FactoryMaker[ReadablePeriod](Days.days(60)) {}

  // ExtSession
  val extSessionExpires = new FactoryMaker[ReadablePeriod](Days.days(90)) {}
  val extSessionCookieName = new FactoryMaker[String]("EXTSESSID") {}
  val extSessionCookiePath = new FactoryMaker[String]("/") {}

  // Permission
  val permissionWilcardToken = new FactoryMaker[String]("*") {}
  val permissionPartDivider = new FactoryMaker[String](":") {}
  val permissionSubpartDivider = new FactoryMaker[String](",") {}
  //val permissionCaseSensitive = new FactoryMaker[Boolean](true) {}

  // MailSender
  val mailerFromAddress = new FactoryMaker[String](Props.get("system.email.address", "gartenhans@agynamix.de")) {}
  val mailerFromAddrName = new FactoryMaker[Box[String]](Full("Unser Gartenverein")) {}

  // FileService
  val documentService = new FactoryMaker[DocumentService](
    if (Props.get("document.service.type", "file") == "file") {
      new FileDocumentService(Props.get("document.service.file.root").openOrThrowException("Property 'document.service.file.root' not defined."))
    } else {
      throw new UnsupportedOperationException("Only file service currently supported")
    }
  ){}

  def init(): Unit = {}

  val fileDownloadApiKey = Props.get("file_download.api.key").openOrThrowException("No Fiel Download API Key defined in property file")

}

object AuthUtil {
  def tryo[T](f: => T): Box[T] = {
    try {
      f match {
        case null => Empty
        case x => Full(x)
      }
    } catch {
      case e: Throwable => Failure(e.getMessage, Full(e), Empty)
    }
  }

  def fancyEmail(name: String, email: String): String = "%s <%s>".format(name, email)
}

/*
 * User gets sent here after a successful login.
 */
object LoginRedirect extends SessionVar[Box[String]](Empty) {
  override def __nameSalt = Helpers.nextFuncName
}
