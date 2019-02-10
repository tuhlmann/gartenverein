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
import com.agynamix.garten.model.PropertyTax
import com.agynamix.garten.model.RecipientListType


object PropertyTaxes extends StructuredMetaSnippet(PropertyTax) {

  lazy val listMenuParam =
    Menu.param[PropertyTax]("OnePropertyTax", Loc.LinkText(a => Text(a.id.get.toString)), id => PropertyTax.find(id),
        (obj: PropertyTax) => obj.id.get.toString) / "property_tax" / *

}

class PropertyTaxes extends StructuredFormSnippet[PropertyTax](PropertyTaxes, PropertyTaxModalDialog, PropertyTaxDetailView) {

  lazy val listPaginator = new OffsetPaginator(this) {

    implicit val manifest = Manifest.classType[PropertyTax](PropertyTax.getClass)

    def masterTableFields = List(
        DbField(PropertyTax.name, true, true, true),
        DbField(PropertyTax.tax, true, true, true)
      )
  }

  def listElement(user: User, selFunc: PropertyTax=>JsCmd)(obj: PropertyTax) = {
    listRecordElements(user, obj, selFunc) &
    bindEdit("@edit", obj) &
    bindRemove("@remove", obj)
  }

  def doBindRemoveAction(obj: PropertyTax) = {
    ajaxRemoveAction(obj, "Datensatz löschen",
        "Möchten Sie die Grundsteuer '%s (%s %%)' tatsächlich löschen?".format(obj.name.get, obj.tax.get))
  }

}

abstract class PropertyTaxLiftScreen extends StructuredLiftScreen[PropertyTax](PropertyTaxes) {

  def objViewPermissions(obj: PropertyTax)   = Permissions.RoleView :: Nil
  def objCreatePermissions(obj: PropertyTax) = Permissions.RoleCreate :: Nil
  def objEditPermissions(obj: PropertyTax)   = Permissions.RoleEdit :: Nil
  def objDeletePermissions(obj: PropertyTax) = Permissions.RoleDelete :: Nil

  tfield("Allgemein", "input-xlarge"   , screenVar.is.name)
  tfield("Allgemein", "input-mini"     , screenVar.is.tax)
  tfield("Allgemein", "input-mini"     , screenVar.is.sortOrder)
  tfield("Allgemein", "input-sxxlarge" , screenVar.is.note)


  def createDataHolder(user: User, activeClient: Box[Client]) =
    activeClient.map(c => PropertyTax.createInstance(c)) openOrThrowException("Need a Client")

}

object PropertyTaxModalDialog extends PropertyTaxLiftScreen {

  val formName = "propertyTaxModal"

  override def dialogTitle = if (IsNewRecord) "Neue Grundsteuer" else "Grundsteuer bearbeiten"
  override val screenLegend = "Bitte füllen Sie alle Felder, um eine Grunsteuer anzulegen"

  def onFinish(data: PropertyTax): Unit = {
    S.notice("Verarbeitung erfolgreich.")
    snippetInstance.is.foreach(_.addOrUpdateModelObj(data))
  }

}

object PropertyTaxDetailView extends PropertyTaxLiftScreen {

  val formName = "propertyTaxDetail"
  override val formTemplateName = DEFAULT_DETAIL_VIEW_TPL
  override def isReadOnly(data: PropertyTax) = true

  def onFinish(data: PropertyTax): Unit = {
  }

}


