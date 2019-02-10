package com.agynamix.garten.snippet

import reactive.Observing
import reactive.BufferSignal
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
import com.agynamix.garten.model.Client
import net.liftweb.util.BaseField
import com.agynamix.garten.model.Role


object Roles extends StructuredMetaSnippet(Role) {

  lazy val listMenuParam =
    Menu.param[Role]("OneMemberRole", Loc.LinkText(a => Text(a.id.get.toString)), id => Role.find(id),
        (obj: Role) => obj.id.get.toString) / "role" / *

}

class Roles extends StructuredFormSnippet[Role](Roles, RolesModalDialog, RolesDetailView) {

  override def overridenDispatch: DispatchIt = {
    case "listDashboard" => listDashboard
  }

  lazy val listPaginator = new OffsetPaginator(this) {

    implicit val manifest = Manifest.classType[Role](Role.getClass)

    def masterTableFields = List(DbField(Role.roleLocalizedName, true, true, true))

//                            DbField(Address.city, true, true, true,
//                                   (f: BaseField, rec: Member) => rec.currentAddress.city.asHtml),
  }

  def listElement(user: User, selFunc: Role=>JsCmd)(obj: Role) = {
    listRecordElements(user, obj, selFunc) &
    "@action [onclick]" #> ajaxInvoke(()=>{
      val href = Roles.listMenuParam.calcHref(obj)
      RedirectTo(href)
    }) &
    bindEdit("@edit", obj) &
    bindRemove("@remove", obj)
  }

  def doBindRemoveAction(obj: Role) = {
    ajaxRemoveAction(obj, "Rolle löschen",
        "Möchten Sie die Rolle '%s' tatsächlich löschen?".format(obj.roleLocalizedName.get))
  }

  def listDashboard = list

}

abstract class RolesLiftScreen extends StructuredLiftScreen[Role](Roles) {

  def objViewPermissions(obj: Role)   = Permissions.RoleView :: Nil
  def objCreatePermissions(obj: Role) = Permissions.RoleCreate :: Nil
  def objEditPermissions(obj: Role)   = Permissions.RoleEdit :: Nil
  def objDeletePermissions(obj: Role) = Permissions.RoleDelete :: Nil

  tfield("Allgemein", "input-xlarge"  , screenVar.is.roleLocalizedName)
  tfield("Allgemein", screenVar.is.memberPermissions)
  tfield("Allgemein", screenVar.is.recipientsPermissions)
  tfield("Allgemein", screenVar.is.gardenPermissions)
  tfield("Allgemein", screenVar.is.notePermissions)
  tfield("Allgemein", screenVar.is.taskPermissions)
  tfield("Allgemein", screenVar.is.eventPermissions)
  tfield("Allgemein", screenVar.is.documentPermissions)
  tfield("Allgemein", screenVar.is.teamPermissions)
  tfield("Allgemein", screenVar.is.invoicePermissions)

  def createDataHolder(user: User, activeClient: Box[Client]) =
    activeClient.map(c => Role.createTeamInstance(c.id.get)) openOrThrowException("Need a Client")

}

object RolesModalDialog extends RolesLiftScreen {

  val formName = "rolesModal"

  override def dialogTitle = if (IsNewRecord) "Neue Rolle" else "Rolle bearbeiten"
  override val screenLegend = "Bitte füllen Sie alle Felder, um eine Rolle hinzuzufügen"

  def onFinish(data: Role): Unit = {
    S.notice("Verarbeitung erfolgreich.")
    snippetInstance.is.foreach(_.addOrUpdateModelObj(data))
  }

}

object RolesDetailView extends RolesLiftScreen {

  val formName = "rolesDetail"
  override val formTemplateName = DEFAULT_DETAIL_VIEW_TPL
  override def isReadOnly(data: Role) = true

  def onFinish(data: Role): Unit = {
  }

}


