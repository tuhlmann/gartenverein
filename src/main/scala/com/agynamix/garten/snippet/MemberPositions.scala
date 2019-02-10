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
import com.agynamix.garten.model.MemberPosition


object MemberPositions extends StructuredMetaSnippet(MemberPosition) {

  lazy val listMenuParam =
    Menu.param[MemberPosition]("OneMemberPosition", Loc.LinkText(a => Text(a.id.get.toString)), id => MemberPosition.find(id),
        (obj: MemberPosition) => obj.id.get.toString) / "member_position" / *

}

class MemberPositions extends StructuredFormSnippet[MemberPosition](MemberPositions, MemberPositionsModalDialog, MemberPositionsDetailView) {

  lazy val listPaginator = new OffsetPaginator(this) {

    implicit val manifest = Manifest.classType[MemberPosition](MemberPosition.getClass)

    def masterTableFields = List(
        DbField(MemberPosition.name, true, true, true)
      )
  }

  def listElement(user: User, selFunc: MemberPosition=>JsCmd)(obj: MemberPosition) = {
    listRecordElements(user, obj, selFunc) &
    bindEdit("@edit", obj) &
    bindRemove("@remove", obj)
  }

  def doBindRemoveAction(obj: MemberPosition) = {
    ajaxRemoveAction(obj, "Datensatz löschen",
        "Möchten Sie den Datensatz '%s' tatsächlich löschen?".format(obj.name.get))
  }

}

abstract class MemberPositionsLiftScreen extends StructuredLiftScreen[MemberPosition](MemberPositions) {

  def objViewPermissions(obj: MemberPosition)   = Permissions.RoleView :: Nil
  def objCreatePermissions(obj: MemberPosition) = Permissions.RoleCreate :: Nil
  def objEditPermissions(obj: MemberPosition)   = Permissions.RoleEdit :: Nil
  def objDeletePermissions(obj: MemberPosition) = Permissions.RoleDelete :: Nil

  tfield("Allgemein", "input-xlarge"  , screenVar.is.name)
  tfield("Allgemein", "input-mini"   , screenVar.is.sortOrder)
  tfield("Allgemein", "input-sxxlarge", screenVar.is.note)


  def createDataHolder(user: User, activeClient: Box[Client]) =
    activeClient.map(c => MemberPosition.createInstance(c)) openOrThrowException("Need a Client")

}

object MemberPositionsModalDialog extends MemberPositionsLiftScreen {

  val formName = "memberPositionsModal"

  override def dialogTitle = if (IsNewRecord) "Neuer Mitgliedsart" else "Mitgliedsart bearbeiten"
  override val screenLegend = "Bitte füllen Sie alle Felder, um eine Mitgliedsart anzulegen"

  def onFinish(data: MemberPosition): Unit = {
    S.notice("Verarbeitung erfolgreich.")
    snippetInstance.is.foreach(_.addOrUpdateModelObj(data))
  }

}

object MemberPositionsDetailView extends MemberPositionsLiftScreen {

  val formName = "memberPositionsDetail"
  override val formTemplateName = DEFAULT_DETAIL_VIEW_TPL
  override def isReadOnly(data: MemberPosition) = true

  def onFinish(data: MemberPosition): Unit = {
  }

}


