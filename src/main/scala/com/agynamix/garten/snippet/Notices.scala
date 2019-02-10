package com.agynamix.garten.snippet

import scala.xml.{NodeSeq, Null, Text}
import net.liftweb._
import net.liftweb.util.Helpers._
import net.liftweb.common._
import net.liftweb.http._
import net.liftweb.http.js._
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js.JE._
import net.liftweb.json._
import net.liftweb.json.JsonDSL._
import net.liftweb.util.Helpers
import net.liftweb.http.js.jquery.JqJE
import net.liftweb.http.LiftRulesMocker.toLiftRules
import net.liftweb.http.js.JsExp.strToJsExp
import net.liftweb.util.Vendor.valToVendor
import scala.xml.NodeSeq.seqToNodeSeq
import net.liftmodules.extras.snippet._
import net.liftmodules.extras.LiftExtras

//object Notices extends BsAlerts

object Notices {
  def render(html: NodeSeq): NodeSeq = {
    // Needed for displaying notices when the page is loaded. ie via redirects
    S.appendJs(LiftExtras.noticeConverter.vend.noticesToJsCmd)
    <div data-alerts="alerts" data-fade="5000"></div>
  }
}


object NoticesOld extends Factory with Loggable {

  case class CloseAlert(id: String, duration: TimeSpan) extends JsCmd {
    def toJsCmd = (After(duration, JqJE.JqId(id) ~> (new JsRaw("alert('close')") with JsMember))).toJsCmd
  }


  /*
   * Config
   */
  val errorTitle = new FactoryMaker[Box[String]](Empty){}
  val warningTitle = new FactoryMaker[Box[String]](Empty){}
  val noticeTitle = new FactoryMaker[Box[String]](Empty){}

  /**
    * Render notices
    */
  def render(html: NodeSeq): NodeSeq = {
    // Capture the value for later AJAX updates
    // TODO: Doesn't work with Ajax as RequestVar
    ShowAllNoticesOld(Helpers.toBoolean(S.attr("showAll") or S.attr("showall")))

    <div id={LiftRules.noticesContainerId} class={S.attr("cls") openOr ""}>{renderNotices()}</div>
  }

  def renderNotices(): NodeSeq = {
    // Determine which formatting function to use based on tag usage
    val notices =
      if (ShowAllNoticesOld.is) S.messages _
      else S.noIdMessages _

    // Compute the formatted set of messages for a given input
    def computeMessageDiv(args: (List[(NodeSeq, Box[String])], NoticeType.Value)): NodeSeq = args match {
      case (messages, noticeType) =>
        // Compute the resulting div
        notices(messages) match {
          case Nil => NodeSeq.Empty
          case msg :: Nil =>
            <div id={noticeType.id} class={cssCls(noticeType)+" fade in"}>
              <a class="close" href="#" data-dismiss="alert">&times;</a>
              <p>{noticeTitle(noticeType).map(t => <strong>{t}</strong>).openOr(Text(""))} {msg}</p>
            </div> ++ genAutoFadeOut(noticeType, noticeType.id)
          case msgs =>
            <div id={noticeType.id} class={cssCls(noticeType)+" fade in"}>
              {noticeTitle(noticeType).map(t => <strong>{t}</strong>).openOr(Text(""))}
              <a class="close" href="#" data-dismiss="alert">&times;</a>
              { msgs.flatMap(e => { <p>{e}</p> }) }
            </div> ++ genAutoFadeOut(noticeType, noticeType.id)
        }
    }


    // Render all three types together
    List((S.errors, NoticeType.Error),
         (S.warnings, NoticeType.Warning),
         (S.notices, NoticeType.Notice)).flatMap(computeMessageDiv)
  }

  def genAutoFadeOut(noticeType: NoticeType.Value, noticeId: String) = noticeType match {
      case NoticeType.Notice => Script(CloseAlert(noticeId, 5 seconds))
      case _ => NodeSeq.Empty
    }
  // Script(Run("$('#%s').alert('close')".format(noticeId)))

  /**
    * Render a single id's notices
    */
  def id(html: NodeSeq): NodeSeq = {
    S.attr("id") match {
      case Full(id) => tailScript(clearJsCmd) ++ <span id={id} class="notices-container">{renderIdMsgs(id)}</span>
      case _ => NodeSeq.Empty
    }
  }

  def renderIdMsgs(id: String): NodeSeq = {
    val errs = S.messagesById(id)(S.errors)
    val warns = S.messagesById(id)(S.warnings)
    val notes = S.messagesById(id)(S.notices)

    val highestLevel: Box[NoticeType.Value] = (errs.length, warns.length, notes.length) match {
      case (e, _, _) if (e > 0) => Full(NoticeType.Error)
      case (_, w, _) if (w > 0) => Full(NoticeType.Warning)
      case (_, _, n) if (n > 0) => Full(NoticeType.Notice)
      case _ => Empty
    }

    <ul>
    {
      List(
        (errs, NoticeType.Error),
        (warns, NoticeType.Warning),
        (notes, NoticeType.Notice)
      ).flatMap { case (messages, noticeType) =>
        messages match {
           case Nil => NodeSeq.Empty
           case msgs =>
             msgs.flatMap(e => {
              <li class={"field-%s".format(lowerCaseTitle(noticeType))}>{e}</li>
            })
         }
      }
    }
    </ul> ++ tailScript(onErrorJsCmd(id, highestLevel))
  }

  def onErrorJsCmd(id: String, level: Box[NoticeType.Value]): JsCmd = level match {
    case Full(noticeType) => Run("""App.views.common.Notices.onError("%s", "%s");""".format(id, lowerCaseTitle(noticeType)))
    case _ => Noop
  }

  def clearJsCmd: JsCmd = Call("""App.views.common.Notices.clear""")

  private def lowerCaseTitle(noticeType: NoticeType.Value): String = noticeType match {
    case NoticeType.Notice => "info"
    case _ => noticeType.lowerCaseTitle
  }

  private def noticeTitle(noticeType: NoticeType.Value): Box[String] = noticeType match {
    case NoticeType.Notice => noticeTitle.vend
    case NoticeType.Error => errorTitle.vend
    case NoticeType.Warning => warningTitle.vend
  }

  private def cssCls(noticeType: NoticeType.Value): String = noticeType match {
    case NoticeType.Notice => "alert alert-info"
    case NoticeType.Error => "alert alert-danger"
    case NoticeType.Warning => "alert alert-warning"
  }

  def tailScript(script: JsCmd): NodeSeq =
    Script(OnLoad(script))

  def init(): Unit = {
    LiftRules.noticesToJsCmd = () => {
      // Compute the global notices first
      val groupMessages: JsCmd = LiftRules.jsArtifacts.setHtml(LiftRules.noticesContainerId, renderNotices()) & clearJsCmd

      // We need to determine the full set of IDs that need messages rendered.
      val idSet: List[String] = (S.idMessages((S.errors)) ++
                   S.idMessages((S.warnings)) ++
                   S.idMessages((S.notices))).map(_._1).distinct

      // Merge each Id's messages and effects into the JsCmd chain
      idSet.foldLeft(groupMessages) {
        (chain,id) => chain &
          LiftRules.jsArtifacts.setHtml(id, renderIdMsgs(id))
      }
    }

    /**
      * LiftScreen overwrites the class on form labels and bootstrap
      * requires the control-label class. So, we disable LiftScreen's
      * overwriting of the class.
      */
    LiftScreenRules.messageStyles.default.set({ nt: NoticeType.Value => nt match {
      case NoticeType.Notice => Null
      case NoticeType.Warning => Null
      case NoticeType.Error => Null
    }})
  }
}

object ShowAllNoticesOld extends SessionVar[Boolean](false)

