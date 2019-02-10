package com.agynamix.garten.snippet

import scala.xml._
import net.liftweb._
import net.liftweb.common._
import net.liftweb.http.{RequestVar, S, SHtml}
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds._
import net.liftweb.util._
import net.liftweb.util.Helpers._
import com.agynamix.garten.model._
import com.agynamix.garten.lib.{GoogleAuthenticator, Blowfisher, Gravatar, MailSender}
import java.text.SimpleDateFormat
import com.agynamix.garten.config.Site
import com.agynamix.garten.config.LoginRedirect
import net.liftmodules.extras.SnippetHelper
import net.liftweb.common.Full
import com.agynamix.garten.model.LoginCredentials
import net.liftweb.http.js.jquery.JqJsCmds.{Show, Hide}

sealed trait UserSnippet extends SnippetHelper with Loggable {

  protected def user: Box[User]

  protected def serve(snip: User => NodeSeq): NodeSeq =
    (for {
      u <- user ?~ "User not found"
    } yield {
      snip(u)
    }): NodeSeq

  protected def serve(html: NodeSeq)(snip: User => CssSel): NodeSeq =
    (for {
      u <- user ?~ "User not found"
    } yield {
      snip(u)(html)
    }): NodeSeq

  def header(xhtml: NodeSeq): NodeSeq = serve { user =>
    <div id="user-header">
      { gravatar(xhtml) }
      <h3>{ name(xhtml) }</h3>
    </div>
  }

  def gravatar(xhtml: NodeSeq): NodeSeq = {
    val size = S.attr("size").map(toInt) openOr Gravatar.defaultSize.vend

    serve { user =>
      Gravatar.imgTag(user.email.get, size)
    }
  }

  //  def username(xhtml: NodeSeq): NodeSeq = serve { user =>
  //    Text(user.username.is)
  //  }

  def name(xhtml: NodeSeq): NodeSeq = serve { user =>
    if (user.fullname.nonEmpty)
      Text("%s (%s)".format(user.fullname, user.email.get))
    else
      Text(user.email.get)
  }

  def title(xhtml: NodeSeq): NodeSeq = serve { user =>
    <lift:head>
      <title lift="Menu.title">{ "AGYNAMIX Garten: %*% - " + user.displayName }</title>
    </lift:head>
  }
}

object CurrentUser extends UserSnippet {
  protected def user = User.currentUser
}

object ProfileLocUser extends UserSnippet {

  protected def user = Site.profileLoc.currentValue

  import java.text.SimpleDateFormat

  val df = new SimpleDateFormat("d MMM, yyyy")

  def profile(html: NodeSeq): NodeSeq = serve(html) { user =>
    val editLink: NodeSeq =
      if (User.currentUser.filter(_.id.get == user.id.get).isDefined)
        <a href={ Site.editProfile.url } class="btn btn-info"><i class="icon-edit icon-white"></i> Profil bearbeiten</a>
      else
        NodeSeq.Empty

    "#id_avatar *" #> Gravatar.imgTag(user.email.get) &
      "#id_name *" #> <h3>{ user.fullname }</h3> &
      "#id_location *" #> user.location.get &
      "#id_whencreated" #> df.format(user.whenCreated.toDate).toString &
      "#id_editlink *" #> editLink
  }
}

/*
object UserLogin extends Loggable {

  def render = {
    // form vars
    var password = ""
    var hasPassword = false
    var remember = User.loginCredentials.is.isRememberMe

    val radios = SHtml.radioElem[Boolean](
      Seq(false, true),
      Full(hasPassword)
    )(it => it.foreach(hasPassword = _))

    def doSubmit(): JsCmd = {
      S.param("email").map(e => {
        val email = e.toLowerCase.trim
        // save the email and remember entered in the session var
        User.loginCredentials(LoginCredentials(email, remember))

        if (hasPassword && email.length > 0 && password.length > 0) {
          User.findByEmail(email) match {
            case Full(user) if (user.password.isMatch(password)) =>
              logger.debug("pwd matched")
              User.logUserIn(user, true)
              if (remember) User.createExtSession(user.id.is.toString)
              else ExtSession.deleteExtCookie()
              (for (uri <- LoginRedirect.get) yield {
                RedirectTo(uri)
              }) openOr {
                RedirectTo(Site.dashboard.url)
              }
            case _ =>
              S.error("Invalid credentials")
              Noop
          }
        }
        else if (hasPassword && email.length <= 0 && password.length > 0) {
          S.error("id_email_err", "Please enter an email")
          Noop
        }
        else if (hasPassword && password.length <= 0 && email.length > 0) {
          S.error("id_password_err", "Please enter a password")
          Noop
        }
        else if (hasPassword) {
          S.error("id_email_err", "Please enter an email")
          S.error("id_password_err", "Please enter a password")
          Noop
        }
        else if (email.length > 0) {
          // see if email exists in the database
          User.findByEmail(email) match {
            case Full(user) =>
              User.sendLoginToken(user)
              User.loginCredentials.remove()
              S.notice("An email has been sent to you with instructions for accessing your account")
              Noop
            case _ =>
              RedirectTo(Site.register.url)
          }
        }
        else {
          S.error("id_email_err", "Please enter an email address")
          Noop
        }
      }) openOr {
        S.error("id_email_err", "Please enter an email address")
        Noop
      }
    }

    def cancel() = S.seeOther(Site.home.url); Noop

    "#id_email [value]" #> User.loginCredentials.is.email &
    "#id_password" #> SHtml.password(password, password = _) &
    "#forgottenPassword" #> <a href="://javascript" onclick="forgot();return false;" >{S ? "login.forgotten.password"}</a> &
    "name=remember" #> SHtml.checkbox(remember, remember = _) &
    "#id_submit" #> S.formGroup(1000) { SHtml.hidden(doSubmit) }
  }
}
*/

class UserLogin extends Loggable {

  var pwAuthenticatedUser: Box[User] = Empty
  var password = ""
  var twoFactorPin = ""
  var remember = User.loginCredentials.is.isRememberMe

  def doUserLogin(user: User): JsCmd = {
    User.logUserIn(user, true)
    if (remember) User.createExtSession(user.id.get.toString())
    else ExtSession.deleteExtCookie()

    if (!Membership.membershipValidates(user)) {
      RedirectTo(Site.myMembership.url, ()=>{
        LoginRedirect.get.foreach(url => {
          MyMembershipDialog.RedirectOnSubmit(url)
          MyMembershipDialog.RedirectOnCancel(url)
        })
        MyMembershipDialog.ScreenLegend.set("Bitte ergänzen Sie die Angaben zu Ihrer Mitgliedschaft")
      })
    } else {
      (for (uri <- LoginRedirect.get) yield {
        RedirectTo(uri)
      }) openOr {
        RedirectTo(Site.dashboard.url)
      }
    }
  }


  def userPasswordForm = {

    def doSubmit(): JsCmd = {
      (S.param("email"), S.param("fpPassword")) match {
        case (Full(e), Full(forgottenPassword_?)) =>
          val email = e.toLowerCase.trim
          // save the email and remember entered in the session var
          User.loginCredentials(LoginCredentials(email, remember))
          if ("false".equals(forgottenPassword_?)) {
            attemptLogin(email)
          } else {
            forgottenPassword(email)
          }
        case otherwise =>
          S.error(S ? "userLogin.doSubmit.error.invalidCredentials")
          Noop
      }
    }

    def attemptLogin(email: String): JsCmd = {
      if (email.length > 0 && password.length > 0) {
        User.findByEmail(email) match {
          case Full(user) if (user.password.isMatch(password)) =>
            if (user.useTwoFactorAuthentication) {
              pwAuthenticatedUser = Full(user)
              Hide("login-fields-user-pw") &
              Show("login-fields-2f-auth")
            } else {
              doUserLogin(user)
            }

          case _ =>
            Logbook.logInvalidLoginAttempt(email)
            S.error(S ? "userLogin.doSubmit.error.invalidCredentials")
            Noop
        }
      } else if (email.length <= 0 && password.length > 0) {
        S.error("id_email_err", S ? "userLogin.doSubmit.error.missing.email")
        Noop
      } else if (password.length <= 0 && email.length > 0) {
        S.error("id_password_err", S ? "userLogin.doSubmit.error.missing.password")
        Noop
      } else {
        S.error("id_email_err", S ? "userLogin.doSubmit.error.missing.email")
        S.error("id_password_err", S ? "userLogin.doSubmit.error.missing.password")
        Noop
      }
    }

    def forgottenPassword(email: String): JsCmd = {
      if (email.isEmpty) S.notice(S ? "userLogin.doSubmit.error.missing.email")
      else {
        User.findByEmail(email).foreach { user =>
          MailSender.sendLoginToken(user)
          User.loginCredentials.remove()
        }
        // deliberately the same message to prevent leaking of valid email addresses
        S.notice(S ? "userLogin.doSubmit.notice.instructions.mailed")
      }

    }

    "#id_email [value]" #> User.loginCredentials.is.email &
    "#id_password" #> SHtml.password(password, password = _) &
    "@remember" #> SHtml.checkbox(remember, remember = _) &
    "@btn-user-pw-submit *+" #> S.formGroup(1000) { SHtml.hidden( doSubmit ) }
  }

  def twoFactorAuthForm = {

    def doTwoFactorSubmit(): JsCmd = {
      (for {
        user <- pwAuthenticatedUser
        encSecret <- user.twoFactorAuthenticationKey.get
        twoFactorCode <- asLong(twoFactorPin)
      } yield {
        val secret = Blowfisher.decrypt(encSecret, user.id.get.toString)
        if (GoogleAuthenticator.checkCode(secret, twoFactorCode, System.currentTimeMillis())) {
          doUserLogin(user)
        } else {
          S.error(S ? "userLogin.do2fSubmit.error.invalidTwoFactorCode")
          Noop
        }
      }) openOr {
        S.error(S ? "userLogin.do2fSubmit.error.noUserFound")
        Hide("login-fields-2f-auth") &
          Show("login-fields-user-pw")
      }
    }

    "#id_2f_pin" #> SHtml.text(twoFactorPin, twoFactorPin = _) &
    "@btn-2f-auth-submit *+" #> S.formGroup(1000) { SHtml.hidden( doTwoFactorSubmit )}
  }

}

object UserTopbar {
  def render = {
    User.currentUser match {
      case Full(user) =>
        <ul class="nav navbar-nav" id="user">
          <li class="dropdown" data-dropdown="dropdown">
            <a href="#" class="dropdown-toggle" data-toggle="dropdown">
              { Gravatar.imgTag(user.email.get, 20) }
              <span>{ user.displayName }</span>
              <b class="caret"></b>
            </a>
            <ul class="dropdown-menu">
              <!--<li><a href={ Site.profileLoc.calcHref(user) }><i class="icon-user"></i> Profil</a></li>-->
              <li><lift:Menu.item name="Account" donthide="true" linktoself="true"><i class="icon-cog"></i> Einstellungen</lift:Menu.item></li>
              <li><lift:Menu.item name="MyDocuments" donthide="true" linktoself="true"><i class="icon-folder-open-alt"></i> Meine Dokumente</lift:Menu.item></li>
              <li><lift:Menu.item name="MyMembership" donthide="true" linktoself="true"><i class="icon-group"></i> Meine Mitgliedschaft</lift:Menu.item></li>
              <li class="divider"></li>
              <li><lift:Menu.item name="Logout" donthide="true"><i class="icon-off"></i>&nbsp;Abmelden</lift:Menu.item></li>
            </ul>
          </li>
        </ul>
      case _ if (S.request.flatMap(_.location).map(_.name).filterNot(it => List("Login", "Register").contains(it)).isDefined) =>
        <ul class="nav navbar-nav">
          <li><a href="/login"><i class="icon-lock icon-white"></i>&nbsp;Anmelden</a></li>
        </ul>
      case _ => NodeSeq.Empty
    }
  }
}
