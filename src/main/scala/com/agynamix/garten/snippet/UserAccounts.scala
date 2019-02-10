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
import com.agynamix.garten.config.RolesDef
import com.agynamix.garten.model.Membership
import com.foursquare.rogue.LiftRogue._
import com.agynamix.garten.model.ConnectionStatus
import com.agynamix.garten.model.SystemUser


object UserAccounts extends StructuredMetaSnippet[User](User) {

  lazy val listMenuParam =
    Menu.param[User]("OneUserAccount", Loc.LinkText(a => Text(a.displayName)), id => User.find(id),
        (obj: User) => obj.id.get.toString) / "user" / *

}

class UserAccounts extends StructuredFormSnippet[User](UserAccounts, UserAccountModalDialog, UserAccountDetailView) {

  lazy val currentClientLoc = ClientsAndAccounts.listMenuParam.toLoc

  lazy val listPaginator = new OffsetPaginator(this) {

    implicit val manifest = Manifest.classType[User](User.getClass)

    def masterTableFields = List(DbField(User.email, true, true, true),
                               DbField(User.lastname, true, true, true),
                               DbField(User.firstname, true, true, true),
                               DbField(User.activeMembership, true, true, true,
                               (f: BaseField, rec: User) => rec.activeMembership.asHtml) )
    override def keyFields(user: User, args: Any*) = {
      //println(s"Search only users for client ${currentClientLoc.currentValue.map(_.name)}")
      val re = dbObjBridge.keyFields(user, currentClientLoc.currentValue)
      re
    }

  }



//  println("Loc Content is")
//  for (v <- currentClientLoc.currentValue) {
//    println("Client: "+v.name)
//  }

  def listElement(user: User, selFunc: User=>JsCmd)(obj: User) = {
    listRecordElements(user, obj, selFunc) &
    "@action [onclick]" #> ajaxInvoke(()=>{
      val href = UserAccounts.listMenuParam.calcHref(obj)
      RedirectTo(href)
    }) &
    bindEdit("@edit", obj) &
    bindRemove("@remove", obj) &
    withPerm("@reset-pwd", myModalScreen(obj).objEditPermissions(obj) :_*){
      ajaxInvoke(()=>{
        MailSender.sendLoginToken(obj)
        S.notice("Passwort Mail gesendet an "+obj.fancyEmail)
      })
    }
  }

  override def clientCreateNew = {
    "@client-name *" #> ""
  }

  def doBindRemoveAction(obj: User) = {
    ajaxRemoveAction(obj, "Nutzeraccount löschen",
        "Möchten Sie den Nutzer '%s, %s' tatsächlich löschen?".format(obj.displayName, obj.email.get))
  }

  override def removeObjectFromDB(obj: User): Boolean = {
    (Membership where (_.userId eqs obj.id.get) bulkDelete_!!!)
    obj.delete_!
  }

}

abstract class UserAccountLiftScreen extends StructuredLiftScreen[User](UserAccounts) {

  def objViewPermissions(obj: User)   = Permissions.MemberView :: Nil
  def objCreatePermissions(obj: User) = Permissions.MemberCreate :: Nil
  def objEditPermissions(obj: User)   = List(Permissions.MemberEdit) // obj.email.get != SystemUser.user.email.get
  def objDeletePermissions(obj: User) = List(Permissions.MemberDelete, obj.email.get != SystemUser.user.email.get)

  tfield(TAB_DEFAULT, "input-xlarge" , screenVar.is.firstname)
  tfield(TAB_DEFAULT, "input-xlarge" , screenVar.is.lastname)
  tfield(TAB_DEFAULT, "input-xlarge" , screenVar.is.email)
  tfield(TAB_DEFAULT, "input-xlarge" , screenVar.is.roles)
  tfield(TAB_DEFAULT, ""             , screenVar.is.activeMembership,
      ClientsAndAccounts.listMenuParam.toLoc.currentValue.isDefined) // readonly if we selected a Client
  tfield(TAB_DEFAULT, "input-xlarge" , screenVar.is.roleInActiveClientConnection)


  def createDataHolder(currentAdminUser: User, activeClient: Box[Client]) = {
    User.createInstance.isNewRecord(true)
  }

  /**
   * If the user is not yet persisted,
   * traverse through attached records (UserClientConnection) and delete them as well
   */
//  override def modalScreenCancelled(data: Membership) = {
////    println("Modal canceled")
//    if (User.find(data.id.get).isEmpty) {
////      println("Delete activeClientConnection")
//      (Membership where (_.userId eqs data.id.get) fetch).foreach(_.delete_!)
//    }
//    super.modalScreenCancelled(data)
//  }

  override def beforeCreateModalScreen(data: User): Unit = {
    for (client <- ClientsAndAccounts.listMenuParam.toLoc.currentValue) {
      //val ms = Client.ensureMembershipExists(data, client, RolesDef.R_TEAM_MEMBER, ConnectionStatus.Invited)
      val ms = Membership.findConnection(data, client) getOrElse {
        logger.info("Creating new TeamMember for user ID "+data.id.get+" and client ID "+client.id.get)
        Membership.createCachedUserInstance(data, client).
                   connectionStatus(ConnectionStatus.Invited).
                   userRole.findOrSet(client.id.get, RolesDef.R_TEAM_MEMBER)
      }
      data.activeMembership.setWithCache(ms)
    }
  }

  override def beforeSave(newUser: User): Boolean = {
    // Save the active client connection, just in case we edited it
//    uhl,29.12.2013: email not stored in Membership anymore
//    for (ms <- newUser.activeMembership.obj) {
//      ms.email(newUser.email.get)
//      ms.save
//    }
//    (for (client <- currentClientLoc.currentValue; acc <- newUser.activeClientConnection.obj) yield {
//      true
//    }) openOr true
    true
  }

}

object UserAccountModalDialog extends UserAccountLiftScreen {

  val formName = "userAccountModal"

  override def dialogTitle = if (IsNewRecord) "Neuer Nutzer" else "Nutzer bearbeiten"
  override val screenLegend = "Bitte füllen Sie alle Felder, um einen Nutzer hinzuzufügen"

  def onFinish(data: User): Unit = {
    println("New record: "+IsNewRecord)
    S.notice("Verarbeitung erfolgreich.")
    if (IsNewRecord) {
      data.password(nextFuncName).password.hashIt
      data.save(true)
    }
    println("Sending Mail?: "+IsNewRecord)
    if (IsNewRecord) {
      println("New Record")
      (for (acc <- data.activeMembership.obj; client <- acc.clientId.obj) yield {
        println("New Record: "+acc.connectionStatus.get)
        if (acc.connectionStatus.get == ConnectionStatus.Invited) {
          MailSender.sendUserAccountWithClientCreated(client, data, acc)
        }
      }) openOr {
        MailSender.sendUserAccountCreated(data)
      }
    } else {
      // Check if the active client connection is in state "Invited", if yes send an invitation
      println("Updated Record")
      for (acc <- data.activeMembership.obj; client <- acc.clientId.obj) {
        println("Updated Record: "+acc.connectionStatus.get)
        if (acc.connectionStatus.get == ConnectionStatus.Invited) {
          MailSender.sendTeamInvitationFromAdmin(client, data, acc)
        }
      }

    }

    snippetInstance.is.foreach(_.addOrUpdateModelObj(data))
  }

}

object UserAccountDetailView extends UserAccountLiftScreen {

  val formName = "userAccountDetail"
  override val formTemplateName = DEFAULT_DETAIL_VIEW_TPL
  override def isReadOnly(data: User) = true

  def onFinish(data: User): Unit = {
  }

}


