package com.agynamix.garten.lib.util

import net.liftweb.util.Helpers._
import net.liftweb.common.Box
import net.liftweb.util.CssSel
import net.liftweb.http.S
import scala.xml.NodeSeq
import net.liftweb.http.Templates
import com.agynamix.garten.lib.Permission
import com.agynamix.garten.model.User
import net.liftweb.util.CanBind
import scala.xml.UnprefixedAttribute
import scala.xml.Null
import net.liftmodules.extras.SnippetHelper
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds.Run

trait SnippetHelpers extends SnippetHelper {

  def hiddenTpl(name: String): NodeSeq = S.runTemplate(List("templates-hidden", name)).openOr(<div>Template not found</div>)
  def platformTpl(name: String): NodeSeq = S.runTemplate(List("templates-hidden", "platform", name)).openOr(<div>Template not found</div>)
  def assocTpl(name: String): NodeSeq = S.runTemplate(List("templates-hidden", "associations", name)).openOr(<div>Template not found</div>)
  def partTpl(name: String): NodeSeq = S.runTemplate(List("templates-hidden", "parts", name)).openOr(<div>Template not found</div>)
  def actionsPartTpl(name: String): NodeSeq = S.runTemplate(List("templates-hidden", "parts", "actions", name)).openOr(<div>Template not found</div>)
  def formPartTpl(name: String): NodeSeq = S.runTemplate(List("templates-hidden", "parts", "form", name)).openOr(<div>Template not found</div>)

  val notExistent = "#notExistent" #> ""

  def onFull[T](valueBox: Box[T])(selFunc: T => CssSel): CssSel = {
    (for (value <- valueBox) yield {
      selFunc(value)
    }) openOr notExistent
  }

  def onCond(cond: => Boolean)(sel: => CssSel): CssSel = {
    if (cond) sel else notExistent
  }

  case class ShowTab(selector: String) extends JsCmd {
    def toJsCmd = """$("%s").tab("show")""".format(selector)
  }

  def showModalTooltips: JsCmd = {
    Run("""$(".modal [data-toggle='tooltip']").tooltip()""")
  }

}

object SnippetHelpers extends SnippetHelpers