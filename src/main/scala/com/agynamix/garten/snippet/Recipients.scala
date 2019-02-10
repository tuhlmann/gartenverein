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
import com.agynamix.garten.model.RecipientList
import com.agynamix.garten.model.RecipientListType


object Recipients extends StructuredMetaSnippet(RecipientList) {

  lazy val listMenuParam =
    Menu.param[RecipientList]("OneRecipientList", Loc.LinkText(a => Text(a.id.get.toString)), id => RecipientList.find(id),
        (obj: RecipientList) => obj.id.get.toString) / "recipients" / *

}

class Recipients extends StructuredFormSnippet[RecipientList](Recipients, RecipientsModalDialog, RecipientsDetailView) {

  override def overridenDispatch: DispatchIt = {
    case "listDashboard" => listDashboard
  }

  lazy val listPaginator = new OffsetPaginator(this) {

    implicit val manifest = Manifest.classType[RecipientList](RecipientList.getClass)

    def masterTableFields = List(
        DbField(RecipientList.listTypeName, true, true, true),
        DbField(RecipientList.name, true, true, true),
        DbField(RecipientList.recipients, true, true, true)
      )

//                            DbField(Address.city, true, true, true,
//                                   (f: BaseField, rec: Member) => rec.currentAddress.city.asHtml),
  }

  def listElement(user: User, selFunc: RecipientList=>JsCmd)(obj: RecipientList) = {
    listRecordElements(user, obj, selFunc) &
    "@action [onclick]" #> ajaxInvoke(()=>{
      val href = Recipients.listMenuParam.calcHref(obj)
      RedirectTo(href)
    }) &
    bindEdit("@edit", obj) &
    bindRemove("@remove", obj)
  }

  def doBindRemoveAction(obj: RecipientList) = {
    ajaxRemoveAction(obj, "Verteiler löschen",
        "Möchten Sie den Verteiler '%s' tatsächlich löschen?".format(obj.name.get))
  }

  def listDashboard = list

}

abstract class RecipientsLiftScreen extends StructuredLiftScreen[RecipientList](Recipients) {

  def objViewPermissions(obj: RecipientList)   = Permissions.RecipientsView :: Nil
  def objCreatePermissions(obj: RecipientList) = Permissions.RecipientsCreate :: Nil
  def objEditPermissions(obj: RecipientList)   = List(Permissions.RecipientsEdit, obj.editable.get)
  def objDeletePermissions(obj: RecipientList) = List(Permissions.RecipientsDelete, obj.editable.get)

  tfield("Allgemein", "input-xlarge"  , screenVar.is.name)
  tfield("Allgemein", ""              , screenVar.is.recipients)


  def createDataHolder(user: User, activeClient: Box[Client]) =
    activeClient.map(c => RecipientList.createInstance(user, c)) openOrThrowException("Need a Client")

}

object RecipientsModalDialog extends RecipientsLiftScreen {

  val formName = "recipientsModal"

  override def dialogTitle = if (IsNewRecord) "Neuer Verteiler" else "Verteiler bearbeiten"
  override val screenLegend = "Bitte füllen Sie alle Felder, um einen Verteiler anzulegen"

  def onFinish(data: RecipientList): Unit = {
    S.notice("Verarbeitung erfolgreich.")
    snippetInstance.is.foreach(_.addOrUpdateModelObj(data))
  }

}

object RecipientsDetailView extends RecipientsLiftScreen {

  val formName = "recipientsDetail"
  override val formTemplateName = DEFAULT_DETAIL_VIEW_TPL
  override def isReadOnly(data: RecipientList) = true

  def onFinish(data: RecipientList): Unit = {
  }

}


