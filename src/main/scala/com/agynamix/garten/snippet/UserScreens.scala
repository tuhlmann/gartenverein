package com.agynamix.garten.snippet

import scala.xml._
import net.liftweb._
import net.liftweb.common._
import net.liftweb.util.Helpers._
import net.liftweb.http.SHtml._
import net.liftweb.http.S
import com.agynamix.garten.model.{Membership, Client, User, NewsletterSubscriber}
import net.liftweb.http.LiftScreen
import net.liftweb.util.{FieldIdentifier, FieldError, BaseField}
import com.agynamix.garten.lib.{MailSender, Gravatar}
import net.liftweb.http.js.JsCmds.SetValById
import net.liftweb.http.js.JsExp.strToJsExp
import com.agynamix.garten.config.Site
import net.liftweb.http.RedirectWithState
import net.liftweb.http.RedirectState
import net.liftweb.record.field.EmailField
import com.foursquare.rogue.LiftRogue._
import com.agynamix.garten.model.User.{regUserFields, CRegUserFields}
import scala.collection.immutable
import net.liftweb.http.js.jquery.JqJsCmds.{Show, Hide}

/*
 * Use for editing the currently logged in user only.
 */
sealed trait BaseCurrentUserScreen extends BaseScreen {
  object userVar extends ScreenVar(User.currentUser.openOrThrowException("We need a user here."))

  override def localSetup {
    Referer(Site.account.url)
  }
}

object AccountScreen extends BaseCurrentUserScreen {
  addFields(() => userVar.is.accountScreenFields)
  
  def formName = "AccountScreen"

  override def localSetup {
    Referer(Site.home.url)
  }

  def finish() {
    userVar.is.save(true)
    S.notice("Änderungen am Account gespeichert.")
  }
}

sealed trait BasePasswordScreen {
  this: LiftScreen =>

  def pwdName: String = "Password"
  def pwdMinLength: Int = 6
  def pwdMaxLength: Int = 32

  val passwordField = password(pwdName, "", trim,
    valMinLen(pwdMinLength, "Das Passwort muss aus mindestens "+pwdMinLength+" Zeichen bestehen"),
    valMaxLen(pwdMaxLength, "Das Passwort darf nicht länger als "+pwdMaxLength+" Zeichen sein"),
    ("tabindex" -> "1"), ("class" -> "form-control")
  )
  val confirmPasswordField = password("Passwort bestätigen", "", trim, ("tabindex" -> "1"), ("class" -> "form-control"))

  def passwordsMustMatch(): Errors = {
    if (passwordField.is != confirmPasswordField.is)
      List(FieldError(confirmPasswordField, "Passwörter müssen übereinstimmen"))
    else Nil
  }
}


object PasswordScreen extends BaseCurrentUserScreen with BasePasswordScreen {
  override def pwdName = "Neues Passwort"
  override def validations = passwordsMustMatch _ :: super.validations

  def formName = "PasswordScreen"
  
  def finish() {
    userVar.is.password(passwordField.is)
    userVar.is.password.hashIt
    userVar.is.save(true)
    S.notice("Neues Passwort gespeichert")
  }
}

/*
 * Use for editing the currently logged in user only.
 */
object ProfileScreen extends BaseCurrentUserScreen {
  
  def formName = "ProfileScreen"
  
  def gravatarHtml =
    <span>
      <div class="gravatar">
        {Gravatar.imgTag(userVar.is.email.get, 60)}
      </div>
      <div class="gravatar">
        <h4>Das Profilbild kann unter <a href="http://gravatar.com" target="_blank">Gravatar.com</a> geändert werden.</h4>
        <p>
          Wir nutzen Ihre Emailadresse {userVar.is.email.get}.
          Es dauert evtl. etwas, bis Veränderungen bei gravatar.com auf unserer Seite verfügbar sind.
        </p>
      </div>
    </span>

  val gravatar = displayOnly("Picture", gravatarHtml)

  addFields(() => userVar.is.profileScreenFields)

  def finish() {
    userVar.is.save(true)
    S.notice("Profiländerungen gespeichert.")
  }
}

// this is needed to keep these fields and the password fields in the proper order
trait BaseRegisterScreen extends BaseScreen {
  object userVar extends ScreenVar(User.regUser.is)

  addFields(() => userVar.is.registerScreenFields)
}

// this is needed to keep these fields and the password fields in the proper order
trait CssBaseRegisterScreen extends BootstrapCssBoundLiftScreen {
  object userVar extends ScreenVar(User.regUser.is)
  object regUserFieldsVar extends ScreenVar(User.regUserFields.is)

  val firstname  = field(userVar.is.firstname, trim, valMinLen(2, "Bitte geben Sie min. 2 Stellen Ihres Vornamens an"), ftrans(removeField(!regUserFieldsVar.is.firstname)))
  val lastname   = field(userVar.is.lastname, trim, valMinLen(2, "Bitte geben Sie min. 2 Stellen Ihres Nachnamens an"), ftrans(removeField(!regUserFieldsVar.is.lastname)))
  val email = field(userVar.is.email, ftrans(readOnly(true), removeField(!regUserFieldsVar.is.email)))

}

/*
 * Use for creating a new user.
 */
object RegisterScreen extends BaseRegisterScreen with BasePasswordScreen {
  override def validations = passwordsMustMatch _ :: super.validations

  def formName = "RegisterScreen"
  
  val rememberMe = builder("", User.loginCredentials.is.isRememberMe, ("tabindex" -> "1"))
    .help(Text("Beim nächsten Mal automatisch anmelden."))
    .make

  override def localSetup {
    Referer(Site.home.url)
  }

  def finish() {
    val user = userVar.is
    user.password(passwordField.is)
    user.password.hashIt
    user.save(true)
    User.logUserIn(user, true)
    if (rememberMe) User.createExtSession(user.id.get.toString)
    S.notice("Vielen Dank für Ihre Registrierung!")
  }
}

/*
 * Use for creating a new user.
 */
object ResetPasswordScreen extends BootstrapCssBoundLiftScreen with CssBaseRegisterScreen with BasePasswordScreen {
  object invitationTokenVar extends ScreenVar(User.regToken.is)

  override def finishButton: Elem = <button>Absenden</button>

  override def validations = passwordsMustMatch _ :: super.validations

  override def localSetup {
    Referer(Site.login.url)
  }

  val formName = "completeInvitationForm"

  def finish() {
    val user = userVar.is
    user.password(passwordField.is)
    user.password.hashIt
    user.save(true)
    invitationTokenVar.is.foreach(_.delete_!)
    //User.logUserIn(user, true)
    RedirectWithState(Site.login.url, RedirectState(() => { S.notice("Um die Registrierung abzuschließen, melden Sie sich bitte mit dem neuen Passwort an.") }))
  }
}

/*
 * A user can claim his account this way.
 */
object ClaimAccountScreen extends BootstrapCssBoundLiftScreen {
  override def finishButton: Elem = <button>Absenden</button>

  override def validations = emailUnique _ :: userFoundAndValid _ :: super.validations

  override def localSetup {
    Referer(Site.login.url)
  }

  val formName = "claimAccountForm"

  private def validateEmail(msg: => String): String => List[FieldError] = {
    s => s match {
      case str if (null ne str) && EmailField.validEmailAddr_?(str) => Nil
      case _ => List(FieldError(currentField.box openOr new FieldIdentifier {}, Text(msg)))
    }
  }

  def emailUnique(): Errors = {
    User.findByEmail(email.get) match {
      case Full(usr) =>
        if ((usr.lastname.get == lastname.get) && (usr.firstname.get == firstname.get)) Nil
        else List(FieldError(email, "Die Emailadresse ist bereits in Verwendung."))
      case _ => Nil
    }
  }

  def userFoundAndValid(): Errors = {
    findUser() match {
      case Full(_) => Nil
      case _ => List(FieldError(new FieldIdentifier {}, "Es wurde kein Datensatz gefunden. Bitte prüfen Sie Ihre Angaben."))
    }
  }

  private def validateClient(msg: => String): String => List[FieldError] = {
    s => s match {
      case str if (null ne str) && Client.findByName(str).isDefined => Nil
      case _ => List(FieldError(currentField.box openOr new FieldIdentifier {}, Text(msg)))
    }
  }

  val lastname   = field("Nachname", "", trim, valMinLen(2, "Bitte geben Sie min. 2 Stellen Ihres Nachnamens an."))
  val firstname  = field("Vorname" , "", trim, valMinLen(2, "Bitte geben Sie min. 2 Stellen Ihres Vornamens an."))
  val email      = field("Email"   , "", trim, validateEmail("Bitte geben Sie eine gültige Emailadresse an."))
  val client     = field("Verein"  , "", trim, validateClient("Der angegebene Verein wurde nicht gefunden."))
  val pin        = field("PIN"     , "", trim, toUpper, valMinLen(8, "Bitte geben Sie die vollständige PIN wie im Anschreiben angegeben, an."))

  def findUser(): Box[(Client, User, Membership)] = {
    Client.findByName(client.get).flatMap(client => {
      val matchingUsers = User where (_.lastname eqs lastname.get) and (_.firstname eqs firstname.get) fetch()
      val theUser = matchingUsers.find(user => {
        user.password.isMatch(pin.get) && Membership.findConnection(user, client).isDefined
      })
      //println("Matching user: "+theUser)
      theUser.flatMap(user => {
        Membership.findConnection(user, client).map(m => (client, user, m))
      })
    })

  }

  def finish() {
    findUser() match {
      case Full((client, user, member)) =>
        logger.info("User found, send invitation token")
        if (user.email.isPlaceholderEmail) {
          user.email.set(email.get)
          user.save(true)
        }
        MailSender.sendUserAccountWithClientCreated(client, user, member)
        AjaxOnDone.set(Hide("claim-account-form") & Show("claim-account-response-success"))
      case _ =>
        logger.warn("No User found")
    }
  }
}

object SubscribeEmail {

  def render = {
    "#id-subscribe-email-in" #> onSubmit(s => {
      if (s.trim.nonEmpty) {
        val record = NewsletterSubscriber.add(s.trim.toLowerCase)
        if (record.validate == Nil) {
          record.save(true)
          S.notice("Vielen Dank! Ihre Adresse wird von uns nicht weitergegeben!")
        } else {
          S.error("Bitte überprüfen Sie das Format der Emailadresse.")
        }
      }
      SetValById("id-subscribe-email-in", "") // clear the input box
    })

  }
}
