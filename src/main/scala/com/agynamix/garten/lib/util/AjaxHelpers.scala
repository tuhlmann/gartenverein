package com.agynamix.garten.lib.util

import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js.JsMember
import net.liftweb.http.js.HtmlFixer
import scala.xml.NodeSeq
import net.liftweb.json.JsonAST.JValue
import net.liftweb.http.js.JE.AnonFunc
import net.liftweb.http.S
import net.liftmodules.extras.LiftExtras
import net.liftweb.http.SHtml
import net.liftweb.http.js.JE.JsRaw

object HtmlJsSeparator extends HtmlFixer {
  case class Html(val html: String) extends JsCmd with JsMember {
    val toJsCmd = "html(" + html + ")"
  }
  case class JS(val js: JsCmd) extends JsCmd with JsMember {
    val toJsCmd = js.toJsCmd
  }
  def apply(content: NodeSeq): (Html, JS) =
    fixHtmlAndJs("inline", content) match {
      case (str, Nil) => (Html(str), JS(Noop))
      case (str, cmds) => (Html(str), JS(cmds.reduceLeft(_ & _)))
    }
}

/**
  * An anonymous JavaScript function that takes JSON data as a parameter and sends it
  * to a callback function via Ajax and executes the return JsCmd on the client.
  */
object JsonCallbackAnonFunc {
  def apply(callback: JValue => JsCmd): AnonFunc = {
    val funcCmd = S.fmapFunc(S.SFuncHolder(s => LiftExtras.parseJsonFunc.vend(s, callback)))(name =>
      SHtml.makeAjaxCall(JsRaw("'" + name + "=' + encodeURIComponent(JSON.stringify(data))"))
    )
    AnonFunc("data", funcCmd)
  }
}

