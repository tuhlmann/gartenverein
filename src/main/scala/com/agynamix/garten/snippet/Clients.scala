package com.agynamix.garten.snippet

import net.liftweb.http.SHtml._
import net.liftweb.http.js.JsCmds._
import net.liftweb.common._
import net.liftweb.util.Helpers._
import net.liftweb.sitemap._
import net.liftweb.sitemap.Loc._
import reactive._
import reactive.web._
import com.agynamix.garten.model.Client
import net.liftweb.util.CssSel
import net.liftweb.http.S
import com.agynamix.garten.model.User
import net.liftweb.http.js.JsCmd
import com.agynamix.garten.model.Membership
import net.liftweb.common.Box.box2Iterable
import com.agynamix.garten.config.Site
import com.agynamix.garten.config.App
import com.agynamix.garten.config.RolesDef
import com.agynamix.garten.config.Permissions
import scala.xml.NodeSeq
import com.agynamix.garten.lib.util.SnippetHelpers
import net.liftmodules.widgets.bootstrap.Modal
import com.agynamix.garten.api.FileUploadInProgress
import com.agynamix.garten.model.Document
import com.agynamix.garten.model.ConnectionStatus

object Clients extends StructuredMetaSnippet[Client](Client) {

//  lazy val listMenuParam =
//    Menu.param[Client]("OneClient", Loc.LinkText(a => Text(a.name.is)), id => Client.find(id),
//        (c: Client) => c.id.is.toString) / "client" / *

  def chooseItem(multi: Boolean, onSel: Any=>JsCmd): JsCmd = {
    new Clients().chooseItem(multi, onSel)
  }

}

class Clients extends StructuredFormSnippet[Client](Clients, ClientModalDialog, ClientDetailView) {

  lazy val listPaginator = new OffsetPaginator(this) {

    implicit val manifest = Manifest.classType[Client](Client.getClass)

    def masterTableFields = List(DbField(Client.registerEntry, true, true, false),
                             DbField(Client.name, true, true, false),
                             DbField(Client.createdAt, true, true, false))
  }

  def listElement(user: User, selFunc: Client=>JsCmd)(client: Client): CssSel = {
    listRecordElements(user, client, selFunc) &
    "@action [onclick]" #> ClientTopbar.switchActiveClient(user, client) &
    bindEdit("@edit", client) &
    bindRemove("@remove", client)
  }

  def doBindRemoveAction(obj: Client) = {
    ajaxRemoveAction(obj, "Verein löschen",
        "Möchten Sie den Verein '%s' tatsächlich löschen?".format(obj.name.get))
  }

  def chooseItem(multi: Boolean, onSel: Any=>JsCmd): JsCmd = {
    def formTemplate(): NodeSeq = SnippetHelpers.formPartTpl("clients-item-chooser")
    val html = renderChooseItem(multi, onSel)(formTemplate)
    Modal(html, "class" -> "max garden-item-chooser", "backdrop" -> false, "keyboard" -> false)
  }

  override def selChosenItem(multi: Boolean, onSel: Any=>JsCmd)(record: Client): JsCmd = ajaxInvoke{ ()=>
      println("Selected: "+record.name)
      onSel(record) &
      Run("$('.garden-item-chooser.modal').modal('hide')")
    }

}

abstract class ClientLiftScreen(_snip: StructuredMetaSnippet[Client]) extends StructuredLiftScreen(_snip) with FieldTransforms[Client] {

  import Permissions._

  def objViewPermissions(obj: Client)   = ClientView :: Nil
  def objCreatePermissions(obj: Client) = ClientCreate :: Nil
  def objEditPermissions(obj: Client)   = ClientEdit :: Nil
  def objDeletePermissions(obj: Client) = List(ClientDelete, !(User.currentUser.flatMap(_.activeMembership.clientId) === obj.id.get))

  tfield("Verein"    , "input-small" ,  screenVar.is.registerEntry)
  tfield("Verein"    ,                  screenVar.is.name)
  tfield("Verein"    ,                  screenVar.is.districtCourt)
  tfield("Verein"    , "input-xlarge" , screenVar.is.phone)
  tfield("Verein"    , "input-xlarge" , screenVar.is.fax)
  tfield("Adresse"   ,                  screenVar.is.street)
  tfield("Adresse"   ,                  screenVar.is.city)
  tfield("Adresse"   , "input-small"  , screenVar.is.zip)
  tfield("Adresse"   ,                  screenVar.is.email)
  tfield("Adresse"   ,                  screenVar.is.web)
  tfield("Bankdaten" , "input-xlarge" , screenVar.is.bankName)
  tfield("Bankdaten" , "input-xlarge" , screenVar.is.accountOwner)
  tfield("Bankdaten" , "input-xlarge" , screenVar.is.swift)
  tfield("Bankdaten" , "input-xlarge" , screenVar.is.iban)
  tfield("Bankdaten" , "input-xlarge" , screenVar.is.deductVat)
  tfield("Logo"      , "input-sxxlarge",screenVar.is.attachments, ftrans(adaptDocTblReadOnly()))


  def createDataHolder(user: User, activeClient: Box[Client]) = Client.createInstance(user)

  override def dialogTitle = if (IsNewRecord) "Neuer Verein" else "Verein bearbeiten"

}

object ClientModalDialog extends ClientLiftScreen(Clients) with ModalLiftScreenSupport[Client] {

  def formName = "clientModal"

  override val screenLegend = "Bitte füllen Sie alle Felder, um einen neuen Mandanten anzulegen"

  override def doFinish(): JsCmd = {
    if (FileUploadInProgress.is) {
      FileUploadInProgress.set(false)
      Noop
    } else {
      super.doFinish
    }
  }

  def onFinish(data: Client) {
    S.notice(if (IsNewRecord) "Neuer Verein angelegt." else "Verein bearbeitet.")

    data.removeMarkedDocuments()
    data.save(true)

    snippetInstance.is.foreach(_.addOrUpdateModelObj(data))
    for (user <- User.currentUser) {

      // Make sure a Membership record exists for that user to that client.
      // If not create one with him as owner
      val ms = Membership.findConnection(user, data) getOrElse {
        logger.info("Creating new Membership entry for user ID "+user.id.get+" and client ID "+data.id.get)
        Membership.createCachedUserInstance(user, data).
                           connectionStatus(ConnectionStatus.Connected).
                           userRole.findOrSet(data.id.get, RolesDef.R_TEAM_OWNER).save(true)
      }

      // Make sure the page is reloaded if the current client is changed
      if (user.activeMembership.clientId === data.id.get) {
        S.redirectTo(com.agynamix.garten.config.App.requestInfo.is.map(_.uri) openOr Site.dashboard.url)
      }
    }
  }

  def uploadDoneCallback(): JsCmd = {
    doAjaxCallback(()=> renderFormCmd )
  }

  override def modalScreenCancelled(data: Client): JsCmd = {
    // Delete files if they have been uploaded
//    println("IS NEW RECORD: "+IsNewRecord.get)
    if (IsNewRecord) data.delete_! else {
      for (existing <- Client.find(data.id.get)) {
        data.attachments.get.filterNot(existing.attachments.get.contains).
        flatMap(Document.find(_)).foreach(doc => {
          Document.documentService.remove(doc.relativeFilePath)
        })
      }
    }
    super.modalScreenCancelled(data)
  }


}

object ClientDetailView extends ClientLiftScreen(Clients) {

  val formName = "clientDetail"
  override val formTemplateName = DEFAULT_DETAIL_VIEW_TPL
  override def isReadOnly(data: Client) = true

  def onFinish(data: Client): Unit = { }

}

