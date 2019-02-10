package com.agynamix.garten.snippet

import scala.xml.NodeSeq
import net.liftweb.common._
import net.liftweb._
import net.liftweb.util.Helpers._
import net.liftweb.util.Props
import scala.xml.Unparsed
import com.agynamix.garten.model.User
import net.liftweb.http.S
import net.liftweb.http.LiftRules
import com.agynamix.garten.lib.Locs
import com.agynamix.garten.lib.Permission
import net.liftweb.sitemap.SiteMap
import com.agynamix.garten.lib.Locs._
import net.liftweb.http.SessionVar
import net.liftweb.http.provider.HTTPCookie
import org.joda.time.Days
import net.liftweb.util.Helpers._
import net.liftweb.http.SHtml
import net.liftweb.http.js.JsCmds.Alert
import net.liftweb.http.js.jquery.JqJsCmds.FadeOut

object ProductionOnly {
  def render(in: NodeSeq): NodeSeq = if (Props.productionMode) in else NodeSeq.Empty
}

object NotInProduction {
  def render(in: NodeSeq): NodeSeq = if (Props.productionMode) NodeSeq.Empty else in
}

object IsSuperUser {
  def render(in: NodeSeq) = if (Locs.RequireSuperUser.test()) in else NodeSeq.Empty
}

object HasClient {
  def render(in: NodeSeq) = if (Locs.RequireHasClient.test()) in else NodeSeq.Empty
}

object HasNoClient {
  def render(in: NodeSeq) = if (!Locs.RequireHasClient.test()) in else NodeSeq.Empty
}

//object HasRole {
//  def render(in: NodeSeq) = {
//    (for (r <- S.attr("r")) yield {
//      if (User.hasRole(r)) in else NodeSeq.Empty
//    }) openOr NodeSeq.Empty
//  }
//}

object HasPerm {
  def render(in: NodeSeq) = {
    (for (p <- S.attr("p")) yield {
      val perm = Permission.fromString(p)
//      logger.debug(s"Perm from ${p}: ${perm.domain}, act: ${perm.actions}")
      if (User.hasPermission(perm)) in else {
//        logger.debug("No Permission. Avl. Perms of user: "+User.authPermissions.map(_.toString).mkString(", "))
        NodeSeq.Empty
      }
    }) openOr NodeSeq.Empty
  }
}

object LacksPerm {
  def render(in: NodeSeq) = {
    (for (p <- S.attr("p")) yield {
      if (User.lacksPermission(Permission.fromString(p))) in else NodeSeq.Empty
    }) openOr NodeSeq.Empty
  }
}

object ThrowException {
  def render = throw new Exception("This is only a test.")
}

/**
 * Use this to generate script tags depending on the run mode.
 * With sbt-closure for instance you use a script.jsm file which contains the
 * different JavaScript files that should be combined.
 * That script lets you use the minified version in production and the single files in
 * development mode, sourced from the same file:
 * &lt;script lift="JavaScriptLoader?prod=/gen/script.js;dev=/js/script.jsm;pf=/js/"&gt;&lt;/script&gt;
 */
object JavaScriptLoader {

  def findFiles(path: String): List[String] = {
    (for (fileList <- LiftRules.loadResourceAsString(path)) yield {
      fileList.split("\\r?\\n").toList.flatMap{ line =>
        if ((!line.trim.isEmpty()) && (!line.trim.startsWith("#"))) {
          Full(line)
        } else Empty
      }
    }) openOr Nil
  }

  def adaptPath(path: String): String = S.attr("pf").map(_ + path) openOr path

  def render(in: NodeSeq): NodeSeq = {
    Props.mode match {
      case Props.RunModes.Production =>
        (for (p <- S.attr("prod")) yield {
          <script src={p} lift="with-resource-id"></script>
        }) openOr in
      case _ =>
        (for (p <- S.attr("dev")) yield {
          if (p.endsWith(".jsm")) {
            findFiles(p).map { file =>
              <script src={adaptPath(file)}></script>
            }
          } else {
            <script src={p} lift="with-resource-id"></script>
          }
        }) openOr in
    }
  }

}

object Html5Shim {
  def render = Unparsed("""
<!-- paulirish.com/2008/conditional-stylesheets-vs-css-hacks-answer-neither/ -->
<!--[if lt IE 7]> <html class="no-js lt-ie9 lt-ie8 lt-ie7" lang="en"> <![endif]-->
<!--[if IE 7]>    <html class="no-js lt-ie9 lt-ie8" lang="en"> <![endif]-->
<!--[if IE 8]>    <html class="no-js lt-ie9" lang="en"> <![endif]-->
<!-- Consider adding a manifest.appcache: h5bp.com/d/Offline -->
<!--[if gt IE 8]><!--><html class="no-js" lang="en"> <!--<![endif]-->
<!--[if lt IE 9]>
<script src="https://html5shim.googlecode.com/svn/trunk/html5.js"></script>
<![endif]-->""")
}

object notOnPage {
  def render(in: NodeSeq): NodeSeq = {
    (for {pageName <- S.attr("name")
          loc <- SiteMap.findLoc(pageName)
          req <- S.request
          currentLoc <- req.location } yield {

      if (loc.name != currentLoc.name) in else NodeSeq.Empty

    }) openOr NodeSeq.Empty
  }
}

object onlyOnPage {
  def render(in: NodeSeq): NodeSeq = {
    (for {pageName <- S.attr("name")
          loc <- SiteMap.findLoc(pageName)
          req <- S.request
          currentLoc <- req.location } yield {

      if (loc.name == currentLoc.name) in else NodeSeq.Empty

    }) openOr NodeSeq.Empty
  }
}

object onlyIfSidebar {
  def render(in: NodeSeq): NodeSeq = {
    (for { req <- S.request
           currentLoc <- req.location } yield {

      if (currentLoc.params.exists(_.isInstanceOf[Sidebar])) in else NodeSeq.Empty

    }) openOr NodeSeq.Empty

  }
}

object classIfKnown {
  def render = {
    if (User.currentUser.isDefined) {
      "^ [class+]" #> S.attr("known")
    } else {
      "^ [class+]" #> S.attr("unknown")
    }
  }
}

object classIfWidth {
  def render = {
    (for { req <- S.request
           currentLoc <- req.location } yield {

      currentLoc.params.find(_.isInstanceOf[Width]) match {
        case Some(FixedWidth) => "^ [class+]" #> S.attr("fixed")
        case _          => "^ [class+]" #> S.attr("fluid")
      }

    }) openOr NodeSeq.Empty
  }
}

object EuCookiePresenter {
  val cookieName = "eu_cookie_check"
  val alertId = nextFuncName
  def render =  {
    (for (cookie <- S.findCookie(cookieName)) yield {
      "@eu-cookie" #> ""      
    }) openOr {
      "@eu-cookie [id]" #> alertId &
      "@btn-place-eu-cookie [onclick]" #> SHtml.ajaxInvoke(()=>{
        val cookie = HTTPCookie(cookieName, "true").setMaxAge(365*24*60*60).setPath("/")
        S.addCookie(cookie)
        new FadeOut(alertId, 0 seconds, 1 second)
      })
    }
  }
  
}
