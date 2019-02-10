package bootstrap.liftweb

import scala.xml.{Null, UnprefixedAttribute}
import net.liftweb._
import common._
import http._
import util._
import util.Helpers._
import com.agynamix.garten._
import config._
import net.liftweb.http.js.JsCmds.Run
import net.liftweb.http.js.JsCmd
import reactive.web.Reactions
import com.agynamix.garten.model.User
import com.agynamix.garten.model.SystemUser
import com.agynamix.garten.config.ErrorHandler
import com.agynamix.garten.config.SmtpMailer
import com.agynamix.garten.lib.Gravatar
import com.agynamix.garten.snippet.Notices
import com.agynamix.garten.model.SystemUser
import java.util.Date
import java.text.SimpleDateFormat
import java.util.Calendar
import java.util.GregorianCalendar
import com.agynamix.garten.config.MongoConfig
import com.agynamix.garten.config.DBSetup
import com.agynamix.garten.snippet.FlexMenu
import com.agynamix.garten.api.FileUpload
import com.agynamix.garten.service.JobScheduler
import net.liftmodules.extras.LiftExtras
import java.util.TimeZone
import org.joda.time.DateTimeZone
import com.agynamix.garten.api.FileDownload
import net.liftweb.http.provider.HTTPRequest
import java.util.Locale
import com.agynamix.garten.api.ApiConfig

/**
 * A class that's instantiated early and run.  It allows the application
 * to modify lift's environment
 */
class Boot extends Loggable {
  def boot {

    System.setProperty("user.timezone", "UTC")
    TimeZone.setDefault(TimeZone.getTimeZone("UTC"))
    DateTimeZone.setDefault(DateTimeZone.UTC)

    logger.info("Run Mode: "+Props.mode.toString)

    // init mongodb
    MongoConfig.init()

    DBSetup.run()

    // init auth-mongo
    GardenConfig.authUserMeta.default.set(User)
    GardenConfig.loginTokenAfterUrl.default.set(Site.password.url)
    GardenConfig.siteName.default.set("unser-gartenverein.de")
    GardenConfig.systemEmail.default.set(SystemUser.user.email.get)
    GardenConfig.systemName.default.set(SystemUser.user.fullname)

    GardenConfig.init()

    // For S.loggedIn_? and TestCond.loggedIn/Out builtin snippet
    LiftRules.loggedInTest = Full(() => User.isLoggedIn)

    // checks for ExtSession cookie
    LiftRules.earlyInStateful.append(User.testForExtSession)

    // Gravatar
//    Gravatar.defaultImage.default.set("mm")
    Gravatar.defaultImage.default.set("http://localhost:8080/media/img/profile/gravatar_default.jpg")

    // config an email sender
    SmtpMailer.init

    // where to search snippet
    LiftRules.addToPackages("com.agynamix.garten")
    LiftRules.addToPackages("com.agynamix.invoice")
//    ResourceServer.allow {
//      // case "fobo" :: tail => true
//      case _ => true
//    }

    LiftRules.snippetDispatch.prepend {
      case "menu" => FlexMenu
      case "Menu" => FlexMenu
    }

    // set the default htmlProperties
    LiftRules.htmlProperties.default.set((r: Req) => new Html5Properties(r.userAgent))

    // Build SiteMap
    LiftRules.setSiteMap(Site.siteMap)

    LiftRules.resourceNames = Site.siteResourceNames :: LiftRules.resourceNames

    // Error handler
    ErrorHandler.init

    // 404 handler
    LiftRules.uriNotFound.prepend(NamedPF("404handler") {
      case (req, failure) =>
        NotFoundAsTemplate(ParsePath(List("404"), "html", false, false))
    })

    // Show the spinny image when an Ajax call starts
    LiftRules.ajaxStart =
      Full(() => LiftRules.jsArtifacts.show("ajax-spinner").cmd)

    // Make the spinny image go away when it ends
    LiftRules.ajaxEnd =
      Full(() => LiftRules.jsArtifacts.hide("ajax-spinner").cmd)

    // Force the request to be UTF-8
    LiftRules.early.append(_.setCharacterEncoding("UTF-8"))

    // Use custom code for notices

    // Init Extras
    LiftExtras.init()
    LiftRules.addToPackages("net.liftmodules.extras")

    //Notices.init()

    LiftRules.localeCalculator = (request: Box[HTTPRequest]) => request.flatMap(_.locale).openOr(Locale.GERMANY)

    // Enable Reactive Web
    Reactions.init()

    // FIXME: Remove this functionality, Use Loc information instead
    LiftRules.earlyInStateful.append((reqBox) => reqBox match {
      // Save the page request in a RequestVar so we can access it from ajax calls
      case Full(req) if (req.request != null) && (!req.ajax_?) => {
        //println("REQ: "+req.uri)
        try {
          //if (req.hostAndPath.trim.nonEmpty && App.requestInfo.is.map(_.uri != req.uri).openOr(true)) App.requestInfo(Full(RequestInfo(req.hostAndPath, req.uri)))
          if (App.requestInfo.is.map(_.uri != req.uri).openOr(true)) App.requestInfo(Full(RequestInfo(req.uri)))
        } catch {
          case e: Exception => e.printStackTrace()
        }
      }
      case _ =>
    })

//    Redirect is now done with a LocParam
//    LiftRules.dispatch.append {
//      // index redirect to dashboard if user is logged in
//      case Req("index" :: Nil, _, _) if (User.isAuthenticated) => () => {
//        println("Redirect to "+ Site.dashboard.url)
//        RedirectResponse(Site.dashboard.url)
//      }
//    }
//
//    implicit def responseToBoxResponse(response: LiftResponse): Box[LiftResponse] = Full(response)

    LiftRules.maxMimeFileSize = 10485760L
	  LiftRules.maxMimeSize     = 10485760L
	  //Make sure we don't put stuff in memory for uploads
	  LiftRules.handleMimeFile = OnDiskFileParamHolder.apply

    LiftRules.logUnreadRequestVars = false
    
    ApiConfig.init()

//    LiftRules.progressListener = {
//      val opl = LiftRules.progressListener
//      val ret: (Long, Long, Int) => Unit =
//        (a, b, c) => {
//          // println("progress listener "+a+" plus "+b+" "+c)
//          // Thread.sleep(100) -- demonstrate slow uploads
//          opl(a, b, c)
//        }
//      ret
//    }

    JobScheduler.init();

  }
}
