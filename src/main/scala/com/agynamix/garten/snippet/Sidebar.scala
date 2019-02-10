package com.agynamix.garten.snippet

import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.common.Loggable
import com.agynamix.garten.lib.util.SnippetHelpers
import scala.xml._
import scala.xml.transform._
import net.liftweb.http.S
import com.agynamix.garten.lib.util.XmlHelpers
import scala.xml.NodeSeq.seqToNodeSeq
import com.agynamix.garten.config.GardenConfig
import com.agynamix.garten.config.App
import com.agynamix.garten.config.Site
import net.liftweb.sitemap.Loc
import com.agynamix.garten.lib.Locs._
import shapeless.headOption

object Sidebar extends SnippetHelpers with XmlHelpers with Loggable {

  private def snarfLastItem: String =
    (for (r <- S.request) yield r.path.wholePath.last) openOr ""

  private def snarfPath: List[String] = (for (r <- S.request) yield r.path.wholePath) openOr Nil

  private def lastHrefElement(href: String): String = {
    tryo {
      val pos = href.lastIndexOf("/")
      if (pos > -1) href.substring(pos+1) else ""
    } openOr ""
  }

  private def hrefElements(hrefs: List[String]): List[List[String]] = {
    tryo {
      hrefs.map(_.split("/").toList.filterNot(_.isEmpty()))
    } openOr Nil
  }

  def getSidebarFromLoc(loc: Loc[_]): Box[String] =
    loc.params.map {
      case sb: Sidebar => Full(sb.sidebarTpl)
      case _ => Empty
    }.flatten.headOption

  def render(in: NodeSeq): NodeSeq = {
    (for {req <- S.request
         loc <- req.location
         sidebarTpl <- getSidebarFromLoc(loc)} yield {

      //println("SB: "+sidebarTpl)
      val css = "#content" #> partTpl(sidebarTpl)
      markActiveElement(css(in))
    }) openOr NodeSeq.Empty
  }

  def markActiveElement(dom: NodeSeq): NodeSeq = {

    def isActiveElement(children: NodeSeq): Boolean = {
      val page = snarfPath
      val allHrefsAsListItems = hrefElements((children \\ "@href").toList.map(_.text))
      allHrefsAsListItems.exists{ hrefItem =>
        page.zipAll(hrefItem, "", "").forall{ case (pageEl, linkEl) =>
          (pageEl == linkEl) || (pageEl == "star")
        }
      }
    }

    /**
     * Find the &kt;li&gt; element that contains the href to our current page and mark it as "active"
     */
    object addActiveClass extends RewriteRule {

      override def transform(n: Node): Seq[Node] = {
        n match {
          case el @ Elem(prefix, "li", attribs, scope, children @ _* ) if (isActiveElement(children)) =>
            val e = el.asInstanceOf[Elem] // Scala casts it as Node, don't know why
            if (attributesContainKey(e.attributes, "class")) {
              e.copy(attributes = mapMetaData(e.attributes) {
                case g @ GenAttr(_, "class", Text(v), _) => g.copy(value = Text(v + " active"))
                case other => other
              })
            } else {
              e % ("class" -> "active")
            }
          case other => other
        }
      }
    }

    /**
     * Find the &lt;ul&gt; container that contains the href to our current page and if that container contains
     * a "collapse" class, remove the "out" class and replace it with "in"
     */
    object uncollapseContainer extends RewriteRule {

      override def transform(n: Node): Seq[Node] = {
        n match {
          case el @ Elem(prefix, "ul", attribs, scope, children @ _* )
                    if attributesContainKeyValue(attribs, "class", "collapse") && isActiveElement(children) =>
            val e = el.asInstanceOf[Elem] // Scala casts it as Node, don't know why
//            println("Found Active element: "+children)
            val c: NodeSeq = children

            e.copy(attributes = mapMetaData(e.attributes) {
              case g @ GenAttr(_, "class", Text(v), _) =>
                g.copy(value = Text{if (v.contains("out")) v.replace("out", "in") else v + " in"})
              case other => other
            })
          case other => other
        }
      }
    }

    object transform extends RuleTransformer(addActiveClass, uncollapseContainer)
    transform(<div id="application-sidebar">{dom}</div>)
  }

}