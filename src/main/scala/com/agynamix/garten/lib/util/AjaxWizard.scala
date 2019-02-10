package com.agynamix.garten.lib.util

import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JsCmds._
import com.agynamix.garten.model.User
import net.liftweb.http.js.jquery.JqJsCmds._
import com.agynamix.garten.lib.util.SnippetHelpers
import net.liftweb.util.CssSel
import com.agynamix.garten.lib.{Blowfisher, GoogleAuthenticator}
import scala.xml._
import net.liftweb.common.Full
import net.liftweb.http.js.JsCmds.SetHtml
import net.liftweb.http.js.JsCmds.SetHtml
import net.liftweb.http.js.JsCmds.SetHtml
import net.liftweb.http.js.JsCmds.SetHtml


trait AjaxWizard extends StatefulSnippet with SnippetHelpers {

  override def dispatch = {
    case wizardId if findWizardById(wizardId).isDefined =>
      findWizardById(wizardId).openOrThrowException("Opps").render
  }

  /**
   * The user needs to define (lazy val) a list of steps for this
   * wizard in the order they occur
   */
  def wizardSteps: List[AjaxWizardStep]

  val stepDivIdentifier = nextFuncName

  def previousOf(who: AjaxWizardStep): Box[AjaxWizardStep] = findWizardNeighborsById(who.wizardId)._1

  def nextTo(who: AjaxWizardStep): Box[AjaxWizardStep] = findWizardNeighborsById(who.wizardId)._3

  def findWizardNeighborsById(id: String): (Box[AjaxWizardStep], Box[AjaxWizardStep], Box[AjaxWizardStep]) = {
    val foundOpt = wizardSteps.zipWithIndex.find{ case (step, idx) => step.wizardId == id }
    (for (found <- foundOpt) yield {
      val prev = if (found._2 > 0) Full(wizardSteps(found._2 - 1)) else Empty
      val next = if (found._2 < wizardSteps.size-1) Full(wizardSteps(found._2 + 1)) else Empty
      (prev, Full(found._1), next)
    }) getOrElse (Empty, Empty, Empty)
  }

  def findWizardById(id: String): Box[AjaxWizardStep] = {
    findWizardNeighborsById(id)._2
  }

  def transitionToNext(me: AjaxWizardStep): JsCmd = {
    (for (next <- nextTo(me)) yield {
      SetHtml(stepDivIdentifier, next.renderHtml())
    }) openOr Noop
  }

  def transitionToPrevious(me: AjaxWizardStep): JsCmd = {
    (for (prev <- previousOf(me)) yield {
      SetHtml(stepDivIdentifier, prev.renderHtml())
    }) openOr Noop
  }

  def wrapScreen(screen: NodeSeq): NodeSeq = {
    <div id={stepDivIdentifier}>{screen}</div>
  }

}

trait AjaxWizardStep extends SnippetHelpers {

  lazy val _wrapperTemplate = formPartTpl("default-ajax-screen")

  /**
   * Override to inject your own template
   * @return the template that wraps the inner screen
   */
  def wrapperTemplate: NodeSeq = _wrapperTemplate

  val myFormId: String = nextFuncName
  def owner: AjaxWizard
  def wizardId: String

  def isInitialStep = false

  def renderStep: CssSel

  def renderCommon: CssSel = {
    "form" #> {ns: NodeSeq => {
      new XForm(myFormId).render(ns)
    }} &
      "form *+" #> S.formGroup(1000) { SHtml.hidden( onBtnNext ) }

  }

  var myNodeSeq = NodeSeq.Empty
  def render(in: NodeSeq): NodeSeq = {
    myNodeSeq = in
    if (isInitialStep) {
      owner.wrapScreen(renderHtml)
    } else {
      NodeSeq.Empty
    }
  }

  def renderHtml(): NodeSeq = {
    val css = renderCommon andThen renderStep
    wrapStep(css(myNodeSeq))
  }

  def btnPrevLabel(): NodeSeq = {
    <span>&lt; Zurück</span>
  }

  def btnPrevClass() = "btn-danger"
  def btnNextClass() = "btn-primary"

  def btnNextLabel(): NodeSeq = {
    <span>Weiter &gt;</span>
  }

  def onBtnNext(): JsCmd

  def onBtnPrev(): JsCmd = {
    owner.transitionToPrevious(this)
  }

  def wrapStep(step: NodeSeq): NodeSeq = {
    val css =
      "#ajax-screen-content" #> step &
        "@btn-prev-label" #> btnPrevLabel &
        "@btn-next-label" #> btnNextLabel &
        "@btn-prev [class+]" #> btnPrevClass &
        "@btn-next [class+]" #> btnNextClass &
        "@btn-next [onclick]" #> SHtml.makeAjaxCall(LiftRules.jsArtifacts.serialize(myFormId)).toJsCmd &
        (for (previous <- owner.previousOf(this)) yield {
          "@btn-prev [onclick]" #> SHtml.ajaxInvoke(()=>onBtnPrev())
        }).openOr {
          "@btn-prev" #> ""
        }

    css(wrapperTemplate)
  }

}


/**
 * This object is the default handler for the &lt;lift:form&gt; tag, which
 * is used to perform AJAX submission of form contents. If the "onsubmit"
 * attribute is set on this tag, then the contents there will be run prior
 * to the actual AJAX call. If a "postsubmit" attribute is present on the
 * tag, then its contents will be executed after successful submission of
 * the form.
 */
class XForm(val itsFormId: String = nextFuncName) extends DispatchSnippet {

  def dispatch : DispatchIt = {
    case "ajax" => render _
  }

  def render(kids: NodeSeq) : NodeSeq = {
    // yeah it's ugly, but I'm not sure
    // we could do it reliably with pattern matching
    // dpp Oct 29, 2010
    if (kids.length == 1 &&
      kids(0).isInstanceOf[Elem] &&
      (kids(0).prefix eq null) &&
      kids(0).label == "form") {
      new Elem(null, "form", addAjaxForm , TopScope, kids(0).child :_*)
    } else {
      Elem(null, "form", addAjaxForm, TopScope, kids : _*)
    }
  }

  private def addAjaxForm: MetaData = {

    val id = itsFormId

    val attr = S.currentAttrsToMetaData(name => name != "id" && name != "onsubmit" && name != "action" && name != "form")

    val pre = S.attr.~("onsubmit").map(_.text + ";") getOrElse ""

    val post = S.attr.~("postsubmit").map("function() { " + _.text + "; }")

    val ajax: String = pre + SHtml.makeAjaxCall(LiftRules.jsArtifacts.serialize(id), AjaxContext.js(post)).toJsCmd + ";" + "return false;"

    new UnprefixedAttribute("id", Text(id),
      new UnprefixedAttribute("action", Text("javascript://"),
        new UnprefixedAttribute("onsubmit", Text(ajax), attr)))
  }
}

