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
import com.agynamix.garten.model.PropertyValue
import com.agynamix.garten.model.RecipientListType
import com.agynamix.garten.model.PropertyValueType


object PropertyValues extends StructuredMetaSnippet(PropertyValue) {

  lazy val listMenuParam =
    Menu.param[PropertyValue]("OnePropertyValue", Loc.LinkText(a => Text(a.id.get.toString)), id => PropertyValue.find(id),
        (obj: PropertyValue) => obj.id.get.toString) / "property_value" / *

}

class PropertyValues extends StructuredFormSnippet[PropertyValue](PropertyValues, PropertyValuesModalDialog, PropertyValuesDetailView) {

  lazy val listPaginator = new OffsetPaginator(this) {

    implicit val manifest = Manifest.classType[PropertyValue](PropertyValue.getClass)

    def masterTableFields = List(
        DbField(PropertyValue.property, true, true, true),
        DbField(PropertyValue.propertyType, true, true, true),
        DbField(PropertyValue.stringValue, true, true, true)
      )
  }

  def listElement(user: User, selFunc: PropertyValue=>JsCmd)(obj: PropertyValue) = {
    listRecordElements(user, obj, selFunc) &
    bindEdit("@edit", obj) &
    bindRemove("@remove", obj)
  }

  def doBindRemoveAction(obj: PropertyValue) = {
    ajaxRemoveAction(obj, "Datensatz löschen",
        "Möchten Sie den Eintrag '%s' tatsächlich löschen?".format(obj.property.get))
  }

}

abstract class PropertyValuesLiftScreen extends StructuredLiftScreen[PropertyValue](PropertyValues) {

  def objViewPermissions(obj: PropertyValue)   = Permissions.InvoiceView :: Nil
  def objCreatePermissions(obj: PropertyValue) = Permissions.InvoiceCreate :: Nil
  def objEditPermissions(obj: PropertyValue)   = Permissions.InvoiceEdit :: Nil
  def objDeletePermissions(obj: PropertyValue) = Permissions.InvoiceDelete :: Nil

  tfield("Allgemein", "input-xlarge"   , screenVar.is.property)
  tfield("Allgemein", "input-xlarge"   , screenVar.is.propertyType)
  tfield("Allgemein", "input-xlarge"   , screenVar.is.stringValue)
  tfield("Allgemein", "input-sxxlarge" , screenVar.is.note)


  def createDataHolder(user: User, activeClient: Box[Client]) =
    activeClient.map(c => PropertyValue.createInstance(user, c)) openOrThrowException("Need a Client")

}

object PropertyValuesModalDialog extends PropertyValuesLiftScreen {

  val formName = "propertyValueModal"

  override def dialogTitle = if (IsNewRecord) "Neuer Eintrag" else "Eintrag bearbeiten"
  override val screenLegend = "Bitte füllen Sie alle Felder, um einen Eintrag anzulegen"

  def onFinish(data: PropertyValue): Unit = {
    S.notice("Verarbeitung erfolgreich.")
    snippetInstance.is.foreach(_.addOrUpdateModelObj(data))
  }

}

object PropertyValuesDetailView extends PropertyValuesLiftScreen {

  val formName = "propertyValueDetail"
  override val formTemplateName = DEFAULT_DETAIL_VIEW_TPL
  override def isReadOnly(data: PropertyValue) = true

  def onFinish(data: PropertyValue): Unit = {
  }

}


