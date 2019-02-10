package com.agynamix.garten.snippet

import reactive.Observing
import reactive.BufferSignal
import net.liftweb.http.SHtml._
import net.liftweb.http.js.JsCmds._
import net.liftweb.common._
import net.liftweb.util._
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
import net.liftweb.http.PaginatorSnippet
import scala.xml.NodeSeq
import net.liftweb.util.CssSel
import java.util.Date
import net.liftweb.http.FieldBinding
import com.agynamix.garten.lib.field.GenderType
import net.liftweb.http.js.JsCmd
import com.agynamix.garten.config.Permissions._
import com.agynamix.garten.config.Permissions
import net.liftweb.util.BaseField
import org.bson.types.ObjectId
import net.liftweb.util.FieldError
import net.liftweb.util.Props
import com.agynamix.garten.config.Site
import net.liftweb.http.SessionVar
import com.agynamix.garten.lib.MailSender
import org.joda.time.DateTime
import com.agynamix.garten.lib.GardenDocumentGenerator
import com.agynamix.garten.lib.GardenDocuments
import scala.collection.mutable.ListBuffer
import com.foursquare.rogue.LiftRogue._
import com.agynamix.garten.api.FileUploadInProgress
import net.liftweb.common.Full
import com.agynamix.garten.lib.Locs._
import net.liftweb.http.js.jquery.JqJsCmds.Show


object Memberships extends StructuredMetaSnippet[Membership](Membership) {

  lazy val listMenu =
    Menu.param[Membership]("OneMembership", Loc.LinkText(a => Text(a.lastname.get)), id => Membership.find(id),
        (obj: Membership) => obj.id.get.toString) / "member" / * >> Hidden >> RequireLoggedIn >> SbManagement

}

class Memberships() extends StructuredFormSnippet[Membership](Memberships, MembershipModalDialog, MembershipDetailView) {

  override def overridenDispatch: DispatchIt = {
    case "listDashboard" => listDashboard
    case "listBirthdays" => listBirthdays
  }

  lazy val listPaginator = new InMemoryPaginator(this) {

    implicit val manifest = Manifest.classType[Membership](Membership.getClass)

    def masterTableFields = List(
                             DbField(Membership.lastname, "Name", true, true, true,
                               (f: BaseField, rec: Membership) => Text(rec.lastAndFirstName)),
                             DbField(Membership.userEmail, true, true, true),
                             DbField(Membership.gardenRef, true, true, true),
                             DbField(Membership.birthday, true, true, true),
                             DbField(Membership.userRole, true, true, true),
                             DbField(Membership.connectionStatus, true, true, true)
                             )

//                            DbField(Address.city, true, true, true,
//                                   (f: BaseField, rec: Member) => rec.currentAddress.city.asHtml),
  }

  def listElement(user: User, selFunc: Membership=>JsCmd)(obj: Membership) = {
    listRecordElements(user, obj, selFunc) &
    "@action [onclick]" #> ajaxInvoke(()=>{
      val href = Memberships.listMenu.calcHref(obj)
      RedirectTo(href)
    }) &
    bindEdit("@edit", obj) &
    bindInvite("@invite", obj) &
    bindRemove("@remove", obj) &
    withPerm("@yearly-bill", InvoiceView)(ajaxInvoke(()=>{
      (for (garden <- obj.gardenRef.objs.headOption) yield {
        val href = Gardens.invoicesPerGardenMenu.calcHref(garden)
        RedirectTo(href)
      }).getOrElse {
        S.error("Für dieses Mitglied wurde kein Garten gefunden.")
        Noop
      }
    }))
  }

  def bindInvite(sel: String, obj: Membership): CssSel = {
    if (obj.connectionStatus.isInvited) {
      (for (user <- User.currentUser; currentClient <- obj.clientId.obj; invitee <- obj.userId.obj) yield {
        withPerm(sel, myModalScreen(obj).objCreatePermissions(obj) :_*)(ajaxInvoke(()=>{
          MailSender.sendTeamInvitationFromOwner(currentClient, user, invitee, obj)
          this.addOrUpdateModelObj(obj)
          S.notice(s"Einladung an ${invitee.email.get} mit Rolle ${obj.userRole.roleDisplayName} erneut gesendet.")
        }))
      }) openOr (sel #> "")
    } else sel #> ""
  }

  override def removeObjectFromDB(obj: Membership): Boolean = {
    val otherConns = (Membership where (_.userId eqs obj.userId.get) and (_.id neqs obj.id.get) fetch)
    for (user <- obj.userId.obj) {
      if (otherConns.isEmpty) {
        if (!user.verified.get) {
          user.delete_!
        }
      } else {
        if (user.activeMembership.clientId === obj.clientId.get) {
          user.activeMembership.clear
          user.save(true)
        }
      }
    }
    obj.delete_!
  }

  def doBindRemoveAction(obj: Membership) = {
    ajaxRemoveAction(obj, "Mitglied löschen",
        "Möchten Sie das Mitglied '%s, %s' tatsächlich löschen?".format(obj.lastname.get, obj.firstname.get))
  }

  def listDashboard = list

  def listBirthdays: CssSel = {
    val yearNow = new DateTime().getYear()
    (for { user <- User.currentUser
          clientId <- user.activeMembership.clientId } yield {
      val upcoming = Membership.upcomingBirthdays(clientId, 30, 10)
      if (upcoming.size == 0) {
        "@upcoming-birthdays" #> "Keine Geburtstage gefunden"
      } else {
        "@eachModelObj" #> upcoming.map(ms => {
          "@name *" #> ms.displayName &
          "@birthday *" #> ms.birthday.asHtml &
          "@age *" #> ms.birthday.get.map(bd => { yearNow - new DateTime(bd).getYear() }) &
          "@greeting [onclick]" #> ajaxInvoke(()=>{
            //Alert("Grüße senden...")
            S.notice("Bitte senden Sie Grüße")
            Noop
          })
        })
      }
    }) openOr {
      "@upcoming-birthdays" #> "Keine Geburtstage gefunden 2"
    }
  }

//  override def selChosenItem(multi: Boolean, onSel: Any=>JsCmd)(record: Member): JsCmd = ajaxInvoke{ ()=>
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

abstract class MembershipLiftScreen extends StructuredLiftScreen[Membership](Memberships) with FieldTransforms[Membership] {

  override def hasUploadField = true

  def objViewPermissions(obj: Membership): Seq[ToPermission]   = Permissions.MemberView :: Nil
  def objCreatePermissions(obj: Membership): Seq[ToPermission] = Permissions.MemberCreate :: Nil
  def objEditPermissions(obj: Membership): Seq[ToPermission]   = Permissions.MemberEdit :: Nil
  def objDeletePermissions(obj: Membership): Seq[ToPermission] = Permissions.MemberDelete :: Nil

  tfield("Person", "input-xlarge"     , screenVar.is.member_no)
  tfield("Person", "input-xlarge"     , screenVar.is.lastname)
  tfield("Person", "input-xlarge"     , screenVar.is.firstname)
  tfield("Person", "input-xlarge"     , screenVar.is.tmpEmail)
  tfield("Person", "input-xlarge"     , screenVar.is.userRole)
  tfield("Person", "input-small"      , screenVar.is.gender)
  tfield("Person", ""                 , screenVar.is.birthday)
  tfield("Address", "input-xlarge"    , screenVar.is.street)
  tfield("Address", "input-small"     , screenVar.is.zip)
  tfield("Address", "input-xlarge"    , screenVar.is.city)
  tfield("Address"                    , screenVar.is.phone)
  tfield("Address"                    , screenVar.is.mobile)
  tfield("Association", ""            , screenVar.is.gardenRef)
  tfield("Association", ""            , screenVar.is.memberJoin)
  tfield("Association", ""            , screenVar.is.memberLeave)
  tfield("Misc"                       , screenVar.is.memberType)
  tfield("Misc"                       , screenVar.is.memberPosition)
  tfield("Misc"                       , screenVar.is.isActiveMember)
  tfield("Misc"   , "input-sxxlarge"  , screenVar.is.note)
  tfield("Anlagen", "input-sxxlarge"  , screenVar.is.attachments, ftrans(adaptDocTblReadOnly()))

  def createDataHolder(user: User, activeClient: Box[Client]): Membership =
    activeClient.map(c => Membership.createInstance(c)) openOrThrowException("Need a Client")

}

abstract class AbstractMembershipModalDialog extends MembershipLiftScreen with ModalLiftScreenSupport[Membership] {

  val formName = "memberModal"

  override def dialogTitle = if (IsNewRecord) "Neues Mitglied" else "Mitglied bearbeiten"

  // We validate the screen only after the rest is validated and ok
  override def validate: List[FieldError] = {
    val fieldErrors = screenFields.filter(_.shouldDisplay_?).filter(_.show_?).flatMap(_.validate)
    if (fieldErrors.isEmpty) screenValidate else fieldErrors
  }
  override def validations = setAndValidateUser _ :: super.validations

  def setAndValidateUser(): Errors = {
    val member = screenVar.get
    (for { itsUser <- member.userId.obj } yield {
      var assignedUser = itsUser
      var additionalErrors = ListBuffer[FieldError]()
      if (assignedUser.isNewRecord.get) {
        member.tmpEmail.valueBox match {
          case Full(email) =>
            // Check if another User record with that email address exists.
            // If yes, use that record, override fields with the values from the User record (?)
            // and ask for confirmation again (see #6)
            User.findByEmail(email) match {
              case Full(existingUser) => if (existingUser.id.get != itsUser.id.get) {
                // a user with that email exists
                member.userId(existingUser.id.get)
                member.userId.setCache(existingUser)
                assignedUser = existingUser
                additionalErrors += FieldError(new FieldIdentifier{},
                    "Nutzer existiert bereits. Bitte prüfen Sie die Daten auf Richtigkeit. Bestätigen Sie dann bitte erneut.")
              }
              case _ =>
                println(s"Member has email: "+email); assignedUser.email(email)
            }
          case _ =>
            val tmpEmail = s"""${nextFuncName}@${Props.get("placeholder.email.domain", "example.com")}"""
            println("No Email, set to: "+tmpEmail);
            assignedUser.email(tmpEmail)
        }
        if (User.find(assignedUser.id.get).isEmpty) {
          // Only reset password if user record is new
          assignedUser.password(nextFuncName)
          assignedUser.password.hashIt
        }
        assignedUser.validate ::: additionalErrors.toList
      } else {
        member.tmpEmail.valueBox match {
          case Full(email) if email != itsUser.email.get => itsUser.email(email); itsUser.validate
          case _ => Nil
        }
      }
    }) openOr "Unbekannter Validierungsfehler aufgetreten."
  }

  def onFinish(data: Membership): Unit = {
    S.notice("Verarbeitung erfolgreich.")

    if (IsNewRecord) {
      for { author <- User.currentUser
            client <- data.clientId.obj
            user <- data.userId.obj } {

        for (email <- data.tmpEmail.valueBox) {
          // send email invitation
          println(s"Do Send invitation to ${email}")
          MailSender.sendUserAccountWithClientCreated(client, user, data)
        }

        // create invitation document for user
        GardenDocuments.createUserInvitationLetter(author, client, user, data)

      }
    }

    // Check for documents marked for removal
    data.removeMarkedDocuments()
    data.save(true)

    snippetInstance.is.foreach(_.addOrUpdateModelObj(data))
  }

  /**
   * For file upload purposes
   */
  override def doFinish(): JsCmd = {
    if (FileUploadInProgress.is) {
      FileUploadInProgress.set(false)
      Noop
    } else {
      super.doFinish
    }
  }

  def uploadDoneCallback(): JsCmd = {
    doAjaxCallback(()=>renderFormCmd)
  }

  override def modalScreenCancelled(data: Membership): JsCmd = {
    // Delete files if they have been uploaded
    //    println("IS NEW RECORD: "+IsNewRecord.get)
    if (IsNewRecord) data.delete_! else {
      for (existing <- Membership.find(data.id.get)) {
        data.attachments.get.filterNot(existing.attachments.get.contains).
          flatMap(Document.find(_)).foreach(doc => {
          Document.documentService.remove(doc.relativeFilePath)
        })
      }
    }
    super.modalScreenCancelled(data)
  }


}

object MembershipModalDialog extends AbstractMembershipModalDialog {
  override val screenLegend = "Bitte füllen Sie alle Felder, um ein Mitglied hinzuzufügen"
}

object MyMembershipDialog extends AbstractMembershipModalDialog {

  import Permissions._

  override def objViewPermissions(obj: Membership): Seq[ToPermission]   = List(true)
  override def objCreatePermissions(obj: Membership): Seq[ToPermission] = List(false)
  override def objEditPermissions(obj: Membership): Seq[ToPermission]   = List(true)
  override def objDeletePermissions(obj: Membership): Seq[ToPermission] = List(false)

  object RedirectOnSubmit extends RequestVar(Site.dashboard.url)
  object RedirectOnCancel extends RequestVar(Site.dashboard.url)
  object ScreenLegend extends RequestVar("")

  override val formTemplateName = DEFAULT_FULL_PAGE_EDIT_TPL
  override def screenLegend = ScreenLegend.get

  override def createDataHolder(user: User, activeClient: Box[Client]) = {
    IsNewRecord.set(false)
    user.activeMembership.obj openOr {
      activeClient.map(c => Membership.createInstance(c)) openOrThrowException("Need a Client")
    }
  }

  override def onFinish(data: Membership): Unit = {
    S.redirectTo(RedirectOnSubmit.get)
  }

  override def modalScreenCancelled(data: Membership): JsCmd = {
    super.modalScreenCancelled(data) & RedirectTo(RedirectOnCancel.get)
  }

}

object MembershipDetailView extends MembershipLiftScreen {

  val formName = "memberDetail"
  override val formTemplateName = DEFAULT_DETAIL_VIEW_TPL
  override def isReadOnly(data: Membership) = true

  def onFinish(data: Membership): Unit = {
  }

}

class OneMembership(member: Membership) extends MembershipLiftScreen {

  println("Show Member "+member.displayName+" guid "+CapturedFormGUID.get)

  val formName = "memberDetail"
  override val formTemplateName = DEFAULT_DETAIL_VIEW_TPL
  override def isReadOnly(data: Membership) = true

  override def createDataHolder(user: User, activeClient: Box[Client]) = {
    IsNewRecord.set(false)
    member
  }

  override def clientInitializeForm(data: Membership, formGuid: String, isReadOnly: Boolean) = {
    val re = Show(formGuid)
    println("Show: "+re.toJsCmd)
    Noop
  }

  def onFinish(data: Membership): Unit = {
  }

}



