package com.agynamix.garten.snippet

import net.liftweb.http.js.JsCmds._
import scala.xml.Elem
import net.liftweb.http.S
import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.http.LiftScreen
import com.agynamix.garten.lib.MailSender
import com.agynamix.garten.config.Permissions


object RequestInviteScreen extends BootstrapCssBoundLiftScreen {

  def objCreatePermissions = Permissions.UserSpace :: Nil
  def objEditPermissions   = Permissions.UserSpace :: Nil
  def objDeletePermissions = Permissions.UserSpace :: Nil

  val emailPattern = "^[_A-Za-z0-9-]+(\\.[_A-Za-z0-9-]+)*@[A-Za-z0-9]+(\\.[A-Za-z0-9]+)*(\\.[A-Za-z]{2,})$".r.pattern
  //def emailPattern = ProtoRules.emailRegexPattern.vend

  val name = field("Name", "", trim, valMinLen(5, "Bitte geben Sie Ihren Namen an"), ftrans())
  val email = field("Email Adresse", "", trim, valRegex(emailPattern, "Bitte geben Sie eine Emailadresse an"), ftrans())
  val message = textarea("Anmerkung (optional)", "", trim, ftrans())

  override def finishButton: Elem = <button>Absenden</button>

  // Validation

  def formName = "reqInviteFormContent"

  def finish() {
    MailSender.sendRequestInviteMail(name.is, email.is, message.is)
    S.notice("Vielen Dank für Ihr Interesse! Wir werden Ihre Anfrage umgehend bearbeiten!")
//    AjaxOnDone.set(Run("$('.field-error', '#id-form-contact').hide()"))
    name.set(""); email.set(""); message.set("")
    AjaxOnDone.set(replayForm)
  }
}

