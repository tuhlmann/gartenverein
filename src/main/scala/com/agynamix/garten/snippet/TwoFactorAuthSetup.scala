package com.agynamix.garten.snippet

import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds._
import com.agynamix.garten.model.User
import com.agynamix.garten.lib.util.{AjaxWizardStep, AjaxWizard, SnippetHelpers}
import com.agynamix.garten.lib.{Blowfisher, GoogleAuthenticator}
import scala.xml.Text
import net.liftweb.common.Full

class TwoFactorWizard extends StatefulSnippet with AjaxWizard {

  lazy val wizardSteps = new AuthenticateStep(this, "authenticate") ::
                         new Setup2FStep(this, "setup") ::
                         new Disable2FStep(this, "disable") :: Nil

  /**
   * Variables for the whole wizard process
   */
  var passwordMatches = false

  class AuthenticateStep(val owner: AjaxWizard, val wizardId: String) extends AjaxWizardStep {

    override def isInitialStep = User.currentUser.map(u => !u.useTwoFactorAuthentication) openOr true

    var password = ""

    def onBtnNext(): JsCmd = {
      (for (user <- User.currentUser if user.password.isMatch(password)) yield {
        passwordMatches = true
        transitionToNext(this)
      }) openOr {
        S.error("id_password_err", S ? "userLogin.doSubmit.error.invalidCredentials")
        Noop
      }
    }

    def renderStep = {
      "#id_password" #> SHtml.password(password, password = _)
    }

  }

  class Setup2FStep(val owner: AjaxWizard, val wizardId: String) extends AjaxWizardStep {

    val domain = "unser-gartenverein.de"

    override def btnPrevLabel = Text("Abbrechen")
    override def btnNextLabel = Text("Zwei-Faktor Authentifizierung jetzt aktivieren")

    override def onBtnPrev = {
      S.notice("Einrichten von Zwei-Faktor Authentifizierung abgebrochen")
      RedirectTo("/")
    }

    private def interleave(str: String, count: Int, c: String): String = {
      val sb = new StringBuilder()
      var i = 0
      while (i < str.length) {
        val subLength = (str.length - i) min count
        sb.append(str.substring(i, i+subLength))
        if (str.length > (i + subLength)) {
          sb.append(c)
        }
        i += subLength
      }
      sb.toString()
    }

    var tmpSecret = ""
    def onBtnNext(): JsCmd = {
      println("Submitted")
      (for (user <- User.currentUser if passwordMatches) yield {
        val encrypted = Blowfisher.encrypt(tmpSecret, user.id.get.toString)
        user.twoFactorAuthenticationKey.setBox(Full(encrypted))
        user.save(true)
        S.notice("Zwei-Faktor Authentifizierung wurde erfolgreich eingerichtet")
        RedirectTo("/")
      }) openOr {
        S.error(S ? "userLogin.doSubmit.error.invalidCredentials")
        Noop
      }
    }

    def renderStep = {
      for {
        user <- User.currentUser
        secret <- GoogleAuthenticator.generateSecretKey()
      } yield {
        tmpSecret = secret
        val url = GoogleAuthenticator.getQRBarcodeURL(user.firstname.get, domain, tmpSecret)

        "@qr-code [src]" #> url &
        "@two-f-code-part-user" #> user.firstname.get &
        "@two-f-code-part-domain" #> domain &
        "@two-f-code-part-secret" #> interleave(secret, 4, "-")
      }

    }

  }

  class Disable2FStep(val owner: AjaxWizard, val wizardId: String) extends AjaxWizardStep {

    override def isInitialStep = User.currentUser.map(_.useTwoFactorAuthentication) openOr false

    override def btnNextLabel = Text("Zwei-Faktor Authentifizierung deaktivieren")
    override def btnNextClass = "btn-danger"

    var password = ""

    def onBtnNext(): JsCmd = {
      (for (user <- User.currentUser if user.password.isMatch(password)) yield {
        user.twoFactorAuthenticationKey.setBox(Empty)
        user.save(true)
        S.notice("Zwei-Faktor Authentifizierung wurde erfolgreich deaktiviert")
        RedirectTo("/")
      }) openOr {
        S.error("id_password_err", S ? "userLogin.doSubmit.error.invalidCredentials")
        Noop
      }
    }

    def renderStep = {

      "#id_password" #> SHtml.password(password, password = _)

    }

  }


}
