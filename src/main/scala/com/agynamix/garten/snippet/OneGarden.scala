package com.agynamix.garten.snippet

import com.agynamix.garten.model.Garden
import net.liftweb.common._
import net.liftweb.util.Helpers._
import scala.xml.NodeSeq
import net.liftweb.http.S
import net.liftweb.util.Helpers
import scala.xml.Group
import net.liftweb.sitemap.Loc
import net.liftweb.sitemap.SiteMap
import scala.xml.Elem
import net.liftweb.sitemap.ConvertableLoc
import scala.xml.Text
import net.liftweb.http.RequestVar
import net.liftweb.http.LiftRules
import net.liftweb.sitemap.Menu
import com.agynamix.garten.config.Site
import net.liftweb.sitemap.Menu.ParamMenuable

class OneGarden(garden: Garden) {

  def gardenProperties = {
    "@garden-number" #> ("A"+garden.garden_no.get)
  }

  def listInvoices(text: NodeSeq): NodeSeq = {
    <a href={Gardens.invoicesPerGardenMenu.calcHref(garden)}>{text}</a>
  }

}

object XMenu {

  def findMenuParam(name: String): Box[ParamMenuable[_]] = {
    val re: Option[ParamMenuable[_]] = Site.menus.find {
      case m: ParamMenuable[_] if m.name == name => true
      case _ => false
    }.flatMap {
      case m: ParamMenuable[_] if m.name == name => Some(m)
      case _ => None
    }
    Box(re)
  }

  def item(text: NodeSeq): NodeSeq = {
    val donthide = S.attr("donthide").map(Helpers.toBoolean) openOr false
    val linkToSelf = (S.attr("linkToSelf") or S.attr("linktoself")).map(Helpers.toBoolean) openOr false

    for {
      name <- S.attr("name").toList
    } yield {
      type T = Q forSome {type Q}

      // Builds a link for the given loc
      def buildLink[T](loc : Loc[T]) = {
        Group(SiteMap.buildLink(name, text) match {
          case e : Elem =>
            Helpers.addCssClass(loc.cssClassForMenuItem,
                                e % S.prefixedAttrsToMetaData("a"))
          case x => x
        })
      }

      (S.request.flatMap(_.location), S.attr("param"), SiteMap.findAndTestLoc(name)) match {
         case (_, Full(param), Full(loc: Loc[T])) => {
//           println("ParamMenuable: "+findMenuParam(name))
//           println("Value for %s: %s".format(param, findMenuParam(name).map(_.parser(param))))
           (for {
             menu <- findMenuParam(name)
             pvBox = menu.parser(param) //loc.convert(param)
             pv <- pvBox
             link <- loc.createLink(pv)
           } yield {
             Helpers.addCssClass(loc.cssClassForMenuItem,
                                 <a href={link}>{text}</a> %
                                 S.prefixedAttrsToMetaData("a"))
           }) openOr Text("")
         }

         case (Full(loc), _, _) if loc.name == name => {
           (linkToSelf, donthide) match {
             case (true, _) => buildLink(loc)
             case (_, true) => {
               if (!text.isEmpty) {
                 Group(text)
               } else {
                 Group(loc.linkText openOr Text(loc.name))
               }
             }
             case _ => Text("")
           }
         }

         case (Full(loc), _, _) => buildLink(loc)

         case _ => Text("")
       }
    }
  }

}