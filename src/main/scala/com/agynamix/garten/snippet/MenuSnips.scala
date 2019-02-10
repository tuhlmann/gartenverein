package com.agynamix.garten.snippet

import scala.xml.NodeSeq
import net.liftweb._
import sitemap.Loc.Hidden
import net.liftweb.common._
import net.liftweb.http.{LiftRules, S}
import net.liftweb.util.Helpers._
import net.liftweb.sitemap.Loc
import scala.xml.Text
import net.liftweb.http.LiftRulesMocker.toLiftRules
import scala.xml.NodeSeq.seqToNodeSeq
import com.agynamix.garten.lib.util.SnippetHelpers
import net.liftweb.sitemap.FlexMenuBuilder
import net.liftweb.http.DispatchSnippet
import com.agynamix.garten.lib.Locs._
import net.liftmodules.extras.snippet.BsMenu
import net.liftmodules.extras.SnippetHelper
import net.liftweb.sitemap.SiteMap
import net.liftweb.sitemap.Loc.MenuCssClass

//object GroupMenu extends AppHelpers {
//  def render(in: NodeSeq): NodeSeq = {
//    for {
//      group <- S.attr("group") ?~ "Group not specified"
//      sitemap <- LiftRules.siteMap ?~ "Sitemap is empty"
//      request <- S.request ?~ "Request is empty"
//      curLoc <- request.location ?~ "Current location is empty"
//    } yield ({
//      val currentClass = S.attr("current_class").openOr("active")
//      sitemap.locForGroup(group) flatMap { loc =>
//        if (!isHidden(loc)) {
//          if (curLoc.name == loc.name) {
//            <li class={currentClass}>{buildLink(loc)}</li>
//          } else {
//            <li>{buildLink(loc)}</li>
//          }
//        } else NodeSeq.Empty
//      }
//    }): NodeSeq
//  }
//
//  def isHidden(loc: Loc[_]): Boolean = loc.params.exists(p => p == Hidden)
//
//  def buildLink(loc: Loc[_]): NodeSeq = {
//    val linkText = loc.linkText openOr Text(loc.name)
//    val locIcons = findIcons(loc)
//    val iconSpan = if (locIcons.size > 0) {
//      <i class={locIcons.flatMap(_.cssIconClass).mkString(" ")}></i>
//    } else NodeSeq.Empty
//    <a href={loc.createDefaultLink}>{iconSpan}<span>&nbsp;{linkText}</span></a>
//  }
//
//  def findIcons(loc: Loc[_]): List[LocIcon] = loc.params.filter(p => p.isInstanceOf[LocIcon]).map(_.asInstanceOf[LocIcon])
//
//}

object Menus extends MyBsMenu

trait MyBsMenu extends SnippetHelper {

  /**
    * Produces a menu UL from a group, for use with Bootstrap.
    */
  def group = {
    val menus: NodeSeq =
      for {
        group <- S.attr("group") ?~ "Group not specified"
        sitemap <- LiftRules.siteMap ?~ "Sitemap is empty"
        request <- S.request ?~ "Request is empty"
        curLoc <- request.location ?~ "Current location is empty"
      } yield ({
        val currentClass = S.attr("current_class").openOr("active")
        sitemap.locForGroup(group) flatMap { loc =>
          val nonHiddenKids = loc.menu.kids.filterNot(_.loc.hidden)
          val styles =
            if (curLoc.name == loc.name || loc.menu.kids.exists(_.loc.name == curLoc.name)) currentClass
            else ""

          if (nonHiddenKids.length == 0) {
            <li class={styles+menuCss(loc)}>{buildLink(loc)}</li>
          } else {
            val dropdown: NodeSeq = nonHiddenKids.map { kid =>
              <li>{buildLink(kid.loc)}</li>
            }

            <li class={styles + " dropdown"}>
              <a href="#" class="dropdown-toggle" data-toggle="dropdown">{loc.linkText.openOr(Text("Empty Name"))} <b class="caret"></b></a>
              <ul class="dropdown-menu">{ dropdown }</ul>
            </li>
          }
        }
      }): NodeSeq

    "* *" #> menus
  }


  def buildLink(loc: Loc[_]): NodeSeq = {
    val linkText = loc.linkText openOr Text(loc.name)
    val locIcons = findIcons(loc)
    val iconSpan = if (locIcons.size > 0) {
      <i class={locIcons.flatMap(_.cssIconClass).mkString(" ")}></i>
    } else NodeSeq.Empty
    <a href={loc.createDefaultLink}>{iconSpan}<span>&nbsp;{linkText}</span></a>
  }

  def findIcons(loc: Loc[_]): List[LocIcon] = loc.params.filter(p => p.isInstanceOf[LocIcon]).map(_.asInstanceOf[LocIcon])

  def menuCss(loc: Loc[_]): String = {
    loc.params.filter(p => p.isInstanceOf[ItemCss]).map(_.asInstanceOf[ItemCss]).map(_.css).mkString(" ")
  }
}

object FlexMenu extends FlexMenuBuilder with DispatchSnippet {

  def dispatch: DispatchIt = overridenDispatch orElse net.liftweb.builtin.snippet.Menu.dispatch

  def overridenDispatch: DispatchIt = {
    case "builder" => ignore => render
  }

}