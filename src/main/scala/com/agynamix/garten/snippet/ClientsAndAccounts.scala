package com.agynamix.garten.snippet

import reactive.Observing
import reactive.BufferSignal
import com.agynamix.garten.model.User
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
import com.agynamix.garten.config.Permissions._
import com.agynamix.garten.config.Permissions
import com.agynamix.garten.model.Address
import com.agynamix.garten.model.Client
import net.liftweb.util.BaseField
import com.agynamix.garten.lib.MailSender
import com.agynamix.garten.config.Site
import net.liftweb.sitemap.Loc._
import com.agynamix.garten.lib.Locs._

object ClientsAndAccounts extends StructuredMetaSnippet[Client](Client) {

  lazy val listMenuParam =
    Menu.param[Client]("OneClientAndAccounts", Loc.LinkText(a => Text(a.name.get)), id => Client.find(id),
        (obj: Client) => obj.id.get.toString) / "admin" / "client" / * >> Hidden >> SbManagement >> RequireLoggedIn >> RequireSuperUser

}

class ClientsAndAccounts extends StructuredFormSnippet[Client](ClientsAndAccounts, ClientsAndAccountsModalDialog, ClientsAndAccountsDetailView) {

  lazy val listPaginator = new OffsetPaginator(this) {

    implicit val manifest = Manifest.classType[Client](Client.getClass)

    def masterTableFields = List(DbField(Client.registerEntry, true, true, true),
                                 DbField(Client.name, true, true, true),
                                 DbField(Client.email, true, true, true) )
  }


  def listElement(user: User, selFunc: Client=>JsCmd)(obj: Client) = {
    listRecordElements(user, obj, selFunc) &
    "@action [onclick]" #> ajaxInvoke(()=>{
      val href = ClientsAndAccounts.listMenuParam.calcHref(obj)
      println("HREF: "+href)
      RedirectTo(href)
    }) &
    bindEdit("@edit", obj) &
    bindRemove("@remove", obj)
  }

  def doBindRemoveAction(obj: Client) = {
    ajaxRemoveAction(obj, "Verein löschen",
        "Möchten Sie den Verein '%s, %s' tatsächlich löschen?".format(obj.name.get, obj.email.get))
  }

}

object ClientsAndAccountsModalDialog extends ClientLiftScreen(ClientsAndAccounts) {

  def formName = "clientModal"

  override val screenLegend = "Bitte füllen Sie alle Felder, um einen neuen Mandanten anzulegen"

  def onFinish(data: Client) {
    S.notice("Neuer Mandant angelegt.")
    snippetInstance.is.foreach(_.addOrUpdateModelObj(data))
  }
}

object ClientsAndAccountsDetailView extends ClientLiftScreen(ClientsAndAccounts) {

  val formName = "clientDetail"
  override val formTemplateName = DEFAULT_DETAIL_VIEW_TPL
  override def isReadOnly(data: Client) = true

  def onFinish(data: Client): Unit = {
  }

}


