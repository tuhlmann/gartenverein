package com.agynamix.garten.snippet

import scala.xml._
import net.liftweb._
import common._
import http._
import util.Helpers._
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js.JsCmd
import net.liftweb.http.js.JE
import net.liftmodules.widgets.bootstrap.Modal
import net.liftweb.http.SHtml._
import net.liftweb.http.js.JsCmds._
import net.liftweb.common._
import net.liftweb.util.Helpers._
import net.liftweb.sitemap._
import net.liftweb.sitemap.Loc._
import scala.xml.Text
import net.liftweb.http.RequestVar
import com.agynamix.garten.lib.util.DateHelpers
import com.agynamix.garten.lib.util.SnippetHelpers
import net.liftweb.util.CssSel
import com.agynamix.garten.model.User
import com.agynamix.garten.lib.StructuredDbObject
import java.util.regex.Pattern
import net.liftweb.util.FieldError
import java.util.Date
import net.liftweb.util.BaseField
import net.liftweb.util.LiftFlowOfControlException
import com.agynamix.invoice.model.TextBlock
import net.liftweb.util.AnyVar
import com.agynamix.garten.model.share.MongoIdRecord
import com.agynamix.garten.lib.Permission
import net.liftweb.util.CanBind
import com.agynamix.garten.model.Client
import com.agynamix.garten.config.Permissions._
import net.liftweb.http.S.BinFuncHolder
import com.agynamix.garten.lib.util.FormHelpers
import net.liftweb.mongodb.record.field.BsonRecordListField
import net.liftweb.json._
import net.liftweb.http.CssBoundLiftScreen


/*
 * Base all LiftScreens off this. Currently configured to use bootstrap.
 */
trait BaseScreen extends LiftScreen {
  override val cancelButton = super.cancelButton % ("class" -> "btn btn-default") % ("tabindex" -> "1")
  override val finishButton = super.finishButton % ("class" -> "btn btn-primary") % ("tabindex" -> "1")

  override def additionalAttributes: MetaData = {
    val cssCls = new UnprefixedAttribute("class", "form-horizontal", Null)
    cssCls.append(super.additionalAttributes)
  }

  def displayOnly(fieldName: => String, html: => NodeSeq) =
    new Field {
      type ValueType = String
      override def name = fieldName
      override implicit def manifest = buildIt[String]
      override def default = ""
      override def toForm: Box[NodeSeq] = Full(html)
    }

}

trait BootstrapCssBoundLiftScreen extends CssBoundLiftScreen with SnippetHelpers {
  override protected lazy val cssClassBinding = new BootstrapCssClassBinding
  object noopAction extends ScreenVar[String](mapLocalAction(() => Noop)(s => s)) {
    override lazy val __nameSalt = nextFuncName
  }

  override def defaultToAjax_? : Boolean = true

  def doAjaxCallback(f: () => JsCmd): JsCmd =
    SHtml.makeAjaxCall(
      LiftRules.jsArtifacts.serialize(NextId.get) +
      JE.Str("&" + LocalActionRef.get + "=" + noopAction.get) +
      S.fmapFunc(f)(s => JE.Str("&" + s + "=_"))
    )

  def closeModal(): JsCmd = {
    Run("$('#%s').parents('.modal').modal('hide')".format(FormGUID.is))
  }

  override def defaultFieldNodeSeq: NodeSeq =
    <div class="fieldContainer form-group">
      <label class="col-md-3 control-label top-control-label"></label>
      <div class="col-md-9 control-body top-control-body">
        <div class="controls">
          <span class="value"></span>
          <span class="help-control help-inline"><i class="help-handle icon-question-sign" data-toggle="tooltip"></i></span>
          <div class="errors">
            <div class="error field-error"></div>
          </div>
        </div>
      </div>
    </div>

//  form-control
  override def allTemplate = SavedDefaultXml.get

  class BootstrapCssClassBinding extends CssClassBinding {
    override def label = "control-label"
  }


  def ftrans(stuff: BaseField=>CssSel*): FieldTransform = {
    FieldTransform((field) => {
      errchk(field) &
      transformHelp(field) &
      stuff.map(_(field)).foldLeft(SnippetHelpers.notExistent)(_ & _)
    })
  }

  def readOnly(readOnly: => Boolean)(field: BaseField): CssSel = {
    if (readOnly) {
      ".controls" #> <div class="form-control-static">{field.asHtml}</div> &
      ".remove-if-readonly" #> ""
    } else {
      SnippetHelpers.notExistent
    }
  }

/*
  def readOnly(readOnly: => Boolean)(field: BaseField): CssSel = {
    if (readOnly) {
      ".value [readonly]" #> "readonly" &
      ".value [disabled]" #> "disabled" &
      ".value [class+]" #> "disabled" &
      ".value [name]" #> "" &
      ".remove-if-readonly" #> ""
    } else {
      SnippetHelpers.notExistent
    }
  }
*/

  def removeField(remove: => Boolean)(field: BaseField): CssSel = {
    if (remove) {
      ".control-group" #> ""
    } else SnippetHelpers.notExistent
  }

  def addCls(addCls: String*)(field: BaseField): CssSel = {
    (if (addCls.size > 0) {
      ".value [class+]" #> addCls.mkString(" ")
    } else {
      SnippetHelpers.notExistent
    })
  }

  //       ".controls [class+]" #> addCls.mkString(" ")


  def errchk(field: BaseField): CssSel =
    if (hasErrors(field)) {
      ".form-group [class+]" #> "has-error"
    } else {
      "#NotExistent" #> ""
    }

  def hasErrors(field: BaseField) = S.errors.exists(t => t._2.isDefined && t._2 == field.uniqueFieldId)

  def transformHelp(field: BaseField): CssSel = {
    field.helpAsHtml match {
      case Full(html) =>
        ".help-handle [title]" #> html.text &
        ".form-control [style+]" #> "display:inline-block;"
      case _ => ".help-control" #> ""
    }
  }

  def cls(clsName: String): FieldTransform = FieldTransform((field) => {".value [class+]" #> clsName})


  def renderFinishButtonJsCmd(finishId: String): String = {
    (if (ajaxForms_?) {
      clientRenderAjaxFinishButtonJsCmd(finishId).toJsCmd
    } else {
      "document.getElementById(" + finishId.encJs + ").submit()"
    })
  }

  def clientRenderAjaxFinishButtonJsCmd(finishId: String): JsCmd = {
    SHtml.makeAjaxCall(LiftRules.jsArtifacts.serialize(finishId))
  }

  /**
   * Override the renderHtml to hook our own client-side
   * mechanisms in.
   */
  override protected def renderHtml(): NodeSeq = {
    val finishId = NextId.get
    val cancelId = nextFuncName

    val theScreen = this

    val finishButton = theScreen.finishButton % ("onclick" -> renderFinishButtonJsCmd(finishId))

    val cancelButton: Elem = theScreen.cancelButton %
      ("onclick" ->
        (if (ajaxForms_?) {
          SHtml.makeAjaxCall(LiftRules.jsArtifacts.serialize(cancelId)).toJsCmd
        } else {
          "document.getElementById(" + cancelId.encJs + ").submit()"
        }))

    val url = S.uri

    def fieldBinding(field: BaseField): Box[FieldBinding] =
      field match {
        case f: Field => f.binding
        case _ => Empty
      }

    def fieldTransform(field: BaseField): List[BaseField => NodeSeq => NodeSeq] =
      field match {
        case f: Field => f.transforms
        case _ => Nil
      }

    renderAll(
      Empty, //currentScreenNumber: Box[NodeSeq],
      Empty, //screenCount: Box[NodeSeq],
      Empty, // wizardTop: Box[Elem],
      theScreen.screenTop, //screenTop: Box[Elem],
      theScreen.screenFields.filter(_.shouldDisplay_?).flatMap(f =>
        if (f.show_?) List(ScreenFieldInfo(f, f.displayHtml, f.helpAsHtml, f.toForm, fieldBinding(f), fieldTransform(f))) else Nil), //fields: List[ScreenFieldInfo],
      Empty, // prev: Box[Elem],
      Full(cancelButton), // cancel: Box[Elem],
      Empty, // next: Box[Elem],
      Full(finishButton), //finish: Box[Elem],
      theScreen.screenBottom, // screenBottom: Box[Elem],
      Empty, //wizardBottom: Box[Elem],
      finishId -> doFinish _,
      Empty,
      cancelId -> (() => {
        redirectBack()
      }), //cancelId: (String, () => Unit),
      theScreen,
      ajaxForms_?)
  }

  /**
   * Override doFinish to hook our own JsCmd in that jumps to the correct tab with the first error
   */
  override protected def doFinish(): JsCmd= {
    val fMap: Map[String, () => JsCmd] = LocalActions.get.get
    if (! LocalAction.get.isEmpty)
      fMap.get(LocalAction.get) map (_()) getOrElse {
        logger.error("No local action available with that binding: "+LocalAction.get)
        Noop
      }
    else {
      validate match {
        case Nil =>
          val snapshot = createSnapshot
          PrevSnapshot.set(Full(snapshot))
          finish()
          redirectBack()
        case xs => {
          S.error(xs)
          if (ajaxForms_?) {
            replayForm & OnLoad(renderOnError(xs))
          } else {
            Noop
          }
        }
      }
    }
  }

  def renderOnError(errors: List[FieldError]): JsCmd = Noop

}

abstract class StructuredLiftScreen[T <: MongoIdRecord[T]](rec: StructuredMetaSnippet[T]) extends BootstrapCssBoundLiftScreen with FormHelpers {

  import SnippetHelpers._

  val TAB_DEFAULT = "default"

  val DEFAULT_MODAL_TPL = "default-modal-form"
  val DEFAULT_DETAIL_VIEW_TPL = "default-detail-view"
  val DEFAULT_FULL_PAGE_EDIT_TPL = "default-page-form-edit"

  case class FieldTabC(tabName: String, fields: List[Field])

  object screenVar extends ScreenVar[T](createDataHolder) {
    override lazy val __nameSalt = nextFuncName
  }

  object currentDetailViewValue extends RequestVar[Box[T]](Empty) {
    override lazy val __nameSalt = nextFuncName
  }

  object snippetInstance extends RequestVar[Box[StructuredFormSnippet[T]]](Empty) {
    override lazy val __nameSalt = nextFuncName
  }

  object CapturedFormGUID extends RequestVar(initCapturedFormGUID) {
    override lazy val __nameSalt = nextFuncName
  }

  def initCapturedFormGUID = nextFuncName

  override def localSetup() {
    FormGUID.set(CapturedFormGUID.is)
    super.localSetup
  }

  override def renderHtml: NodeSeq = {
    super.renderHtml ++ Script(OnLoad(clientInitializeForm(screenVar.is, CapturedFormGUID.is, IsReadOnly.is)))
  }

  def clientInitializeForm(data: T, formGuid: String, isReadOnly: Boolean): JsCmd = Noop

  var screenTabs: List[FieldTabC] = Nil

  object IsNewRecord extends RequestVar(true) {
    override lazy val __nameSalt = nextFuncName
  }

  def createDataHolder(currentUser: User, activeClient: Box[Client]): T

  def createDataHolder: T = User.currentUser match {
    case Full(user) =>     createDataHolder(user, user.activeMembership.activeClient)
    case _ => throw new IllegalStateException("Need a user")
  }

  /**
   * Called before the data is saved.
   * @returns true if the the called snippet wants the save to proceed, false otherwise
   */
  def beforeSave(data: T): Boolean = true

  /**
   * Called after the data has been saved.
   * @returns Has no effect yet.
   */
  def afterSave(data: T): Boolean = true


  def saveDataHolder(data: T): Unit = {
    if (beforeSave(data)) {
      data.save(true)
    }
    afterSave(data)
  }

  /**
   * Permissions the current user needs to have in order to create a new instance of this object
   */
  def objViewPermissions(obj: T): Seq[ToPermission]

  def isPermittedToViewElement(obj: T): Boolean = objViewPermissions(obj).foldLeft(true)(_ && _.hasPermission)

  /**
   * Permissions the current user needs to have in order to create a new instance of this object
   */
  def objCreatePermissions(obj: T): Seq[ToPermission]

  def isPermittedToCreateElement(obj: T): Boolean = objCreatePermissions(obj).foldLeft(true)(_ && _.hasPermission)

  /**
   * Permissions the current user needs to have in order to edit an existing instance
   */
  def objEditPermissions(obj: T): Seq[ToPermission]

  def isPermittedToEditElement(obj: T): Boolean = objEditPermissions(obj).foldLeft(true)(_ && _.hasPermission)

  /**
   * Permissions the current user needs to have in order to delete an existing instance
   */
  def objDeletePermissions(obj: T): Seq[ToPermission]

  def isPermittedToDeleteElement(obj: T): Boolean = objDeletePermissions(obj).foldLeft(true)(_ && _.hasPermission)


  object IsReadOnly extends ScreenVar[Boolean](isReadOnly(screenVar.is))

  def isReadOnly(obj: T): Boolean = !(objEditPermissions(obj).foldLeft(true)(_ && _.hasPermission))

  override def readOnly(alwaysRO: => Boolean)(field: BaseField): CssSel = {
    super.readOnly(IsReadOnly.is || alwaysRO)(field)
  }

  /**
   * Called just before the modal dialog is created.
   */
  def beforeCreateModalScreen(data: T): Unit = {}

  /**
   * Called when a modal screen's cancel button is pressed.
   */
  def modalScreenCancelled(data: T): JsCmd = {
    Noop
  }

  def withSnippet(block: StructuredFormSnippet[T] => CssSel): CssSel = {
    (for (snippet <- snippetInstance.is) yield {
      block(snippet)
    }) openOr notExistent
  }

  def formTemplateName = DEFAULT_MODAL_TPL
  override def allTemplate = processTpl(SnippetHelpers.formPartTpl(formTemplateName))

  def createDialog = {
    beforeCreateModalScreen(screenVar.is)
    Modal(this.toForm, "keyboard" -> false, "class" -> "max", "role" -> "dialog") & showModalTooltips
  }

  def myMetaSnippet = rec

  def bindViewEditBtn: CssSel = withSnippet(_.bindEdit("@view-tpl-edit", screenVar.is))
  def bindViewRemoveBtn: CssSel = withSnippet(_.bindRemove("@view-tpl-delete", screenVar.is))

  /**
   * Override for specific implementations to do something when a tab is clicked
   */
  def screenTabActivated(tabId: String): JsCmd = { Noop }

  def hideEmptyViewRender: Boolean = true

  def processTpl(in: NodeSeq): NodeSeq = {
    val current = screenVar.is
    val css =
      onCond(hideEmptyViewRender) { "@view-wrapper [style+]" #> "display: none;" } &
      (if (!screenLegend.isEmpty()) {
        "@screenLegend *" #> screenLegend
      } else {
        "@screenLegend" #> ""
      }) &
      "@cancel *" #> cancelButtonName &
      "@cancel [onclick]" #> ajaxInvoke(()=>modalScreenCancelled(current)) &
      bindViewRemoveBtn &
      bindViewEditBtn &
      "@screenFieldContainer" #> {ns: NodeSeq => generateFieldContainer(ns) }
    css(in)
  }

  def formFieldId(fieldName: String) = "%s_%s_field".format(formName, fieldName)

  def formTabId(idx: Int) = s"nav_${formName}_tab_${idx}"

  def generateFieldContainer(in: NodeSeq): NodeSeq = {
    val header = if (screenTabs.size > 1) {
      <ul class="nav nav-tabs">{screenTabs.zipWithIndex.map{case (tab, idx) =>
        val tabId = formTabId(idx)
        <li class={if (idx==0) "active" else ""}>
          <a id={tabId}
             href={s"#${formName}_tab_${idx}"}
             data-toggle="tab"
             onclick={ajaxInvoke(()=>screenTabActivated(tabId))._2.toJsCmd} >{S ? tab.tabName}</a>
        </li>
      }}
      </ul>
    } else NodeSeq.Empty

    val body = if (screenTabs.size > 0) {
      <div class="fields">
        <div class="tab-content">{screenTabs.zipWithIndex.map{case (tab, idx) =>
          <div class={if (idx == 0) "tab-pane active" else "tab-pane"} id={"%s_tab_%s".format(formName, idx)}>
            {tab.fields.map{field =>
              <div id={formFieldId(field.name)}></div>
            }}
          </div>
        }}
        </div>
      </div>
    } else SnippetHelpers.formPartTpl("default-modal-fields")

    <div>
      {header}
      {body}
    </div>

  }

  /**
   * If errors have occured we append this JsCmd to the replayed form
   */
  override def renderOnError(errors: List[FieldError]): JsCmd = {
    (for { error <- errors.headOption
           fieldId <- error.field.uniqueFieldId
           tabIdx <- screenTabs.zipWithIndex.find{ case (tab, idx) =>
                     tab.fields.exists(f => { f.uniqueFieldId === fieldId })}.map(_._2)} yield {
      ShowTab("#"+formTabId(tabIdx))
    }) getOrElse Noop
  }

  def screenLegend: String = ""
  def onFinish(data: T): Unit

  /**
   * Hook in JS to be called after onFinish succeeds
   */
  def onFinishJsCmd(data: T) = closeModal

  def clearView(): JsCmd = {
    //Run("$('.value').val('')")
    FormGUID.set(CapturedFormGUID.is)
    screenVar.set(createDataHolder)
    currentDetailViewValue.set(Empty)
    replayForm
  }

  def finish() {
    try {
      val data = screenVar.is
      saveDataHolder(data)
      AjaxOnDone.set(onFinishJsCmd(data))
      currentDetailViewValue.set(Empty)
      onFinish(data)
      IsNewRecord.set(true)
      S.appendJs(snippetInstance.is.map(s => s.myDetailView(data).setAndRenderView(data)).openOr(Noop))
    //AjaxOnDone.set(Run("$('.field-error').hide()"))
    } catch {
      case e: LiftFlowOfControlException => throw e
      case e: Exception => e.printStackTrace()
    }
  }

  def setAndRenderModal(n: T): JsCmd = {
    doAjaxCallback(() => {
      FormGUID.set(CapturedFormGUID.is)
      screenVar.set(reloadRecord(n).openOr(n))
      IsNewRecord.set(false)
      //screenVar.set(n)
      createDialog
      // replayForm
    })
  }

  def setAndRenderView(n: T): JsCmd = {
    doAjaxCallback(() => {
      FormGUID.set(CapturedFormGUID.is)
//      screenVar.set(reloadRecord(n).openOr(n))
//      println("setAndRender: "+n)
      screenVar.set(n)
      currentDetailViewValue(Full(n))
      replayForm & Run("$('#%s [name=view-wrapper]').show()".format(FormGUID.get)) //& Run("window.scrollTo(0,document.body.scrollHeight)")
    })
  }

  def reloadRecord(rec: T): Box[T] = myMetaSnippet.dbObjBridge.find(rec.id.get)

  def unselectIfObj(rec: T): JsCmd =
    if (currentDetailViewValue.is.map(_.id.get).openOr(-1) == rec.id.get) clearView else Noop

  override def finishButton: Elem = <button style={if (IsReadOnly) "display:none;" else ""}>{finishButtonText}</button>

  def finishButtonText = "Speichern"

  def cancelButtonName = if (IsReadOnly) "Schließen" else "Abbrechen"

  def dialogTitle(): String = ""
  override def screenTop = <h3>{dialogTitle}</h3>

  def toJValue(snippet: StructuredFormSnippet[T])(data: T): JValue = JNothing

  protected def tfield[T](underlying: => BaseField {type ValueType = T},
                         stuff: FilterOrValidate[T]*)(implicit man: Manifest[T]): Field {type ValueType = T} = {
    tfield[T](TAB_DEFAULT, underlying, false, stuff :_*)(man)
  }

  protected def tfield[T](tabName: String, underlying: => BaseField {type ValueType = T},
                         stuff: FilterOrValidate[T]*)(implicit man: Manifest[T]): Field {type ValueType = T} = {
    tfield[T](tabName, "input-xlarge", underlying, false, stuff :_*)(man)
  }

  protected def tfield[T](tabName: String, underlying: => BaseField {type ValueType = T},
                         alwaysReadOnly: Boolean,
                         stuff: FilterOrValidate[T]*)(implicit man: Manifest[T]): Field {type ValueType = T} = {
    tfield[T](tabName, "input-xlarge", underlying, alwaysReadOnly, stuff :_*)(man)
  }

  protected def tfield[T](tabName: String, cssCls: String, underlying: => BaseField {type ValueType = T},
                         stuff: FilterOrValidate[T]*)(implicit man: Manifest[T]): Field {type ValueType = T} = {
    tfield[T](tabName, cssCls, underlying, false, stuff :_*)(man)
  }

  /**
   * Tab Field. Registers the field into a specific tab, adds a FieldBinding
   */
  protected def tfield[T](tabName: String, cssCls: String, underlying: => BaseField {type ValueType = T},
                         alwaysReadOnly: => Boolean,
                         stuff: FilterOrValidate[T]*)(implicit man: Manifest[T]): Field {type ValueType = T} = {
    val addedStuff: List[FilterOrValidate[T]] = FieldBinding(underlying.name) :: stuff.toList
    lazy val f: Field {type ValueType = T} = field[T](underlying, (ftrans(addCls(cssCls), readOnly(alwaysReadOnly)) :: addedStuff):_*)(man)
    val idx = screenTabs.indexWhere(_.tabName == tabName)
    if (idx > -1) {
      screenTabs = screenTabs.replace(idx, screenTabs(idx).copy(fields = screenTabs(idx).fields ::: List(f)))
    } else {
      screenTabs = screenTabs ::: List(FieldTabC(tabName, List(f)))
    }
    f
  }

}


