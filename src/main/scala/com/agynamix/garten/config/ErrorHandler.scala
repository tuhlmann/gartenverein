package com.agynamix.garten.config


import net.liftweb._
import net.liftweb.common.{Loggable, MDC}
import net.liftweb.http.{Factory, LiftRules, RedirectResponse, Req, S, XhtmlResponse}
import net.liftweb.util.Props
import com.agynamix.garten.model.User
import java.net.InetAddress
import net.liftweb.http.LiftRulesMocker.toLiftRules
import net.liftweb.util.Vendor.valToVendor
import scala.Array.canBuildFrom

object ErrorHandler extends Factory with Loggable {
  // config
  val errorUrl = new FactoryMaker[String]("/error") {} // where to send the user when an error occurs

  def init(): Unit = {
    LiftRules.exceptionHandler.prepend {
      case (Props.RunModes.Development, r, e) =>
        logException(r, e)
        XhtmlResponse(
          (<html><body>Exception occured while processing {r.uri}<pre>{showException(e)}</pre></body></html>),
          S.htmlProperties.docType,
          List("Content-Type" -> "text/html; charset=utf-8"),
          Nil,
          500,
          S.legacyIeCompatibilityMode
        )
      case (_, r, e) =>
        logException(r, e)
        RedirectResponse(errorUrl.vend)
    }
  }

  /*
   * Log the exception with some user info.
   */
  def logException(r: Req, e: Throwable) {
    e.printStackTrace()
    import java.net.InetAddress
    val srvr = InetAddress.getLocalHost.getHostName

    MDC.put(("UserId", User.currentUserId openOr "GUEST"))
    MDC.put(("Username", User.currentUser.map(_.displayName) openOr "GUEST"))
    MDC.put(("User Agent", r.userAgent openOr "UNKNOWN"))
    MDC.put(("Server", srvr))
    logger.error("Exception occurred while processing %s".format(r.uri), e)
  }

  /**
  * A utility method to convert an exception to a string of stack traces
  * @param le the exception
  *
  * @return the stack trace
  */
  def showException(le: Throwable): String = {
    val ret = "Message: " + le.toString + "\n\t" +
            le.getStackTrace.map(_.toString).mkString("\n\t") + "\n"

    val also = le.getCause match {
      case null => ""
      case sub: Throwable => "\nCaught and thrown by:\n" + showException(sub)
    }

    ret + also
  }
}