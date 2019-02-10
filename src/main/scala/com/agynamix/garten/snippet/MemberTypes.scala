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
import com.agynamix.garten.model.MemberType
import com.agynamix.garten.model.RecipientListType


object MemberTypes extends StructuredMetaSnippet(MemberType) {

  lazy val listMenuParam =
    Menu.param[MemberType]("OneMemberType", Loc.LinkText(a => Text(a.id.get.toString)), id => MemberType.find(id),
        (obj: MemberType) => obj.id.get.toString) / "member_type" / *

}

class MemberTypes extends StructuredFormSnippet[MemberType](MemberTypes, MemberTypesModalDialog, MemberTypesDetailView) {

  lazy val listPaginator = new OffsetPaginator(this) {

    implicit val manifest = Manifest.classType[MemberType](MemberType.getClass)

    def masterTableFields = List(
        DbField(MemberType.name, true, true, true),
        DbField(MemberType.isContractor, true, true, true)
      )
  }

  def listElement(user: User, selFunc: MemberType=>JsCmd)(obj: MemberType) = {
    listRecordElements(user, obj, selFunc) &
    bindEdit("@edit", obj) &
    bindRemove("@remove", obj)
  }

  def doBindRemoveAction(obj: MemberType) = {
    ajaxRemoveAction(obj, "Datensatz löschen",
        "Möchten Sie den Datensatz '%s' tatsächlich löschen?".format(obj.name.get))
  }

}

abstract class MemberTypesLiftScreen extends StructuredLiftScreen[MemberType](MemberTypes) {

  def objViewPermissions(obj: MemberType)   = Permissions.RoleView :: Nil
  def objCreatePermissions(obj: MemberType) = Permissions.RoleCreate :: Nil
  def objEditPermissions(obj: MemberType)   = Permissions.RoleEdit :: Nil
  def objDeletePermissions(obj: MemberType) = Permissions.RoleDelete :: Nil

  tfield("Allgemein", "input-xlarge"  , screenVar.is.name)
  tfield("Allgemein", "input-mini"    , screenVar.is.isContractor)
  tfield("Allgemein", "input-small"    , screenVar.is.yearlyPayment)
  tfield("Allgemein", "input-mini"    , screenVar.is.sortOrder)
  tfield("Allgemein", "input-sxxlarge", screenVar.is.note)


  def createDataHolder(user: User, activeClient: Box[Client]) =
    activeClient.map(c => MemberType.createInstance(c)) openOrThrowException("Need a Client")

}

object MemberTypesModalDialog extends MemberTypesLiftScreen {

  val formName = "memberTypesModal"

  override def dialogTitle = if (IsNewRecord) "Neuer Mitgliedsart" else "Mitgliedsart bearbeiten"
  override val screenLegend = "Bitte füllen Sie alle Felder, um eine Mitgliedsart anzulegen"

  def onFinish(data: MemberType): Unit = {
    S.notice("Verarbeitung erfolgreich.")
    snippetInstance.is.foreach(_.addOrUpdateModelObj(data))
  }

}

object MemberTypesDetailView extends MemberTypesLiftScreen {

  val formName = "memberTypesDetail"
  override val formTemplateName = DEFAULT_DETAIL_VIEW_TPL
  override def isReadOnly(data: MemberType) = true

  def onFinish(data: MemberType): Unit = {
  }

}


