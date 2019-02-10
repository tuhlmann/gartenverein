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
import com.agynamix.garten.model._
import java.util.Date
import net.liftweb.util.FieldError
import net.liftweb.http.js.JsCmd
import scala.xml.NodeSeq
import net.liftweb.sitemap.Menu.ParamMenuable
import net.liftweb.sitemap.Menu.WithSlash
import com.agynamix.garten.lib.Locs._
import com.agynamix.garten.config.Permissions
import org.bson.types.ObjectId
import com.agynamix.garten.lib.Locs._
import com.agynamix.garten.api.FileUploadInProgress
import net.liftweb.common.Full

object Gardens extends StructuredMetaSnippet[Garden](Garden) {

  lazy val listMenu =
    Menu.param[Garden]("OneGarden", Loc.LinkText(a => Text(a.garden_no.get.toString)),
        selectGarden,
        (obj: Garden) => obj.id.get.toString) / "garden" / *  >> Hidden >> RequireLoggedIn >> SbManagement

  lazy val invoicesPerGardenMenu =
    Menu.param[Garden]("InvoicesPerGarden", Loc.LinkText(a => Text(a.garden_no.get.toString)),
        selectGarden,
        (obj: Garden) => obj.id.get.toString) / "garden" / * / "invoices"  >> Hidden >> RequireLoggedIn >> SbManagement

  def selectGarden(id: String): Box[Garden] = id match {
    case "current" => CurrentGarden.is
    case id => val re = Garden.find(id); CurrentGarden(re); re
  }

}

object CurrentGarden extends RequestVar[Box[Garden]](Empty)


class Gardens extends StructuredFormSnippet[Garden](Gardens, GardenModalDialog, GardenDetailView) {

  import Permissions._

  lazy val listPaginator = new InMemoryPaginator(this) {

    implicit val manifest = Manifest.classType[Garden](Garden.getClass)

    def masterTableFields = List(DbField(Garden.gardenOwners, true, true, true),
                                 DbField(Garden.garden_no, true, true, true),
                                 DbField(Garden.garden_size, true, true, true)
                                )
  }

  def listElement(user: User, selFunc: Garden=>JsCmd)(obj: Garden) = {
     listRecordElements(user, obj, selFunc) &
    "@action [onclick]" #> ajaxInvoke(()=>{
      val href = Gardens.listMenu.calcHref(obj)
      RedirectTo(href)
    }) &
    bindEdit("@edit", obj) &
    bindRemove("@remove", obj)
  }

  def doBindRemoveAction(obj: Garden) = {
    ajaxRemoveAction(obj, "Garten löschen",
        "Möchten Sie den Garten '%s' tatsächlich löschen?".format(obj.garden_no.get))
  }

//  def chooseItem(multi: Boolean, curRecipients: List[ObjectId], onSel: Any=>JsCmd): JsCmd = {
//    selectedElems.++=(curRecipients)
//    def formTemplate(): NodeSeq = SnippetHelpers.formPartTpl("gardens-item-chooser")
//    val html = renderChooseItem(multi, onSel)(formTemplate)
//    Modal(html, "class" -> "max garden-item-chooser", "backdrop" -> false, "keyboard" -> false)
//  }

//  override def selChosenItem(multi: Boolean, onSel: Any=>JsCmd)(record: Garden): JsCmd = ajaxInvoke{ ()=>
//    val b = toggleElementSelected(record.id.is)
//    println("Selected: "+record.id+", state: "+b)
//    record.selected(b)
//    if (multi) {
//      if (b) {
//        Run("$('#chk-%s').attr('checked', 'checked');".format(record.id.is.toString))
//      } else {
//        Run("$('#chk-%s').removeAttr('checked');".format(record.id.is.toString))
//      }
//    } else {
//      onSel(SelectedElements(selectedElems.toList, removedElems.toList)) &
//      Run("$('.members-items-chooser.modal').modal('hide')")
//    }
//  }

}

abstract class GardenLiftScreen extends StructuredLiftScreen[Garden](Gardens) with FieldTransforms[Garden] {

  def objViewPermissions(obj: Garden)   = Permissions.GardenView :: Nil
  def objCreatePermissions(obj: Garden) = Permissions.GardenCreate :: Nil
  def objEditPermissions(obj: Garden)   = Permissions.GardenEdit :: Nil
  def objDeletePermissions(obj: Garden) = Permissions.GardenDelete :: Nil

  override def hasUploadField = true

  tfield("Garten"     , "input-mini"   , screenVar.is.garden_no)
  tfield("Garten"     , "input-mini"   , screenVar.is.garden_size)
  tfield("Garten"     , "input-mini"   , screenVar.is.houseSize)
  tfield("Garten"     , "input-medium" , screenVar.is.idElectricMeter)
  tfield("Garten"     , "input-medium" , screenVar.is.idWaterMeter)
  tfield("Weiteres"   , "input-mini"   , screenVar.is.propertyTax)
  tfield("Weiteres"   ,                  screenVar.is.section_1)
  tfield("Weiteres"   ,                  screenVar.is.section_2)
  tfield("Weiteres"   ,                  screenVar.is.section_3)
  tfield("Freitext 1" , "input-xlarge" , screenVar.is.text_1)
  tfield("Freitext 1" ,                  screenVar.is.text_2)
  tfield("Freitext 1" ,                  screenVar.is.text_3)
  tfield("Freitext 1" ,                  screenVar.is.text_4)
  tfield("Freitext 1" ,                  screenVar.is.text_5)
  tfield("Freitext 2" ,                  screenVar.is.text_6)
  tfield("Freitext 2" ,                  screenVar.is.text_7)
  tfield("Freitext 2" ,                  screenVar.is.text_8)
  tfield("Freitext 2" ,                  screenVar.is.text_9)
  tfield("Freitext 2" ,                  screenVar.is.text_10)
  tfield("Werte"      , "",              screenVar.is.waterMeterReading)
  tfield("Werte"      , "",              screenVar.is.electricityMeterReading)
  tfield("Rechnung"   , "input-margin" , screenVar.is.invoiceItems, ftrans(fullWidthField))
  tfield("Anlagen"    , "input-sxxlarge",screenVar.is.attachments, ftrans(adaptDocTblReadOnly()))

  def createDataHolder(user: User, activeClient: Box[Client]) =
    activeClient.map(c => Garden.createInstance(user, c)) openOrThrowException("Need a Client")

}

object GardenModalDialog extends GardenLiftScreen with ModalLiftScreenSupport[Garden] {

  def formName = "gardenModal"

  override val dialogTitle = "Neuer Garten"
  override val screenLegend = "Bitte füllen Sie alle Felder, um einen neuen Garten anzulegen"

  object AddInvoiceItem extends RequestVar(false)

  override def doFinish(): JsCmd = {
    val cmds =
    ((if (FileUploadInProgress.is) {
      FileUploadInProgress.set(false)
      Full(Noop)
    } else Empty) ::
    (if (AddInvoiceItem) {
      AddInvoiceItem(false)
      Full(screenVar.is.invoiceItems.addOrEditInvoiceItem)
    } else Empty) :: Nil).flatten
    if (cmds.nonEmpty) {
      cmds.foldLeft(Noop)(_ & _)
    } else {
      super.doFinish
    }
  }

  override def onFinish(data: Garden): Unit = {
    S.notice("Neuer Garten angelegt.")

    // Check for documents marked for removal
    data.removeMarkedDocuments()
    data.save(true)

    snippetInstance.is.foreach(_.addOrUpdateModelObj(data))
  }

  def uploadDoneCallback(): JsCmd = {
    doAjaxCallback(()=>renderFormCmd)
  }

  override def modalScreenCancelled(data: Garden): JsCmd = {
    // Delete files if they have been uploaded
    //    println("IS NEW RECORD: "+IsNewRecord.get)
    if (IsNewRecord) data.delete_! else {
      for (existing <- Garden.find(data.id.get)) {
        data.attachments.get.filterNot(existing.attachments.get.contains).
          flatMap(Document.find(_)).foreach(doc => {
          Document.documentService.remove(doc.relativeFilePath)
        })
      }
    }
    super.modalScreenCancelled(data)
  }


}

object GardenDetailView extends GardenLiftScreen {

  val formName = "gardenDetail"

  override def screenLegend: String = {
    val all = Membership.findGardenOwnersContractorsFirst(screenVar.is.id.get)
    val re = all.map(m => {
      s"${m.displayName} (${m.birthday.currentAge}, ${m.memberJoin.memberSince})"
    }).mkString(", ")

    s"Garten Nr. ${screenVar.is.garden_no.get}, ${re}"
  }

  override val formTemplateName = DEFAULT_DETAIL_VIEW_TPL
  override def isReadOnly(data: Garden) = true

  def onFinish(data: Garden): Unit = {}

}
