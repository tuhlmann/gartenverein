package com.agynamix.garten.snippet

import net.liftweb.common._
import net.liftweb.util.Helpers._
import com.agynamix.garten.model.share.MongoIdRecord
import com.agynamix.garten.lib.StructuredDbObject
import net.liftweb.http.{S, DispatchSnippet, GUIDJsExp, SHtml}
import reactive.Observing
import com.agynamix.garten.lib.util.SnippetHelpers
import net.liftweb.http.js.JsCmd
import reactive.Var
import reactive.BufferSignal
import net.liftweb.util.CssSel
import net.liftweb.http.SHtml._
import net.liftmodules.widgets.bootstrap.Bs3ConfirmRemoveDialog
import scala.xml.NodeSeq
import net.liftweb.http.js.JsCmds._
import scala.xml.Elem
import com.agynamix.garten.model.User
import reactive.web.Repeater
import reactive.web.Cell
import com.agynamix.garten.config.Permissions._
import org.bson.types.ObjectId
import net.liftmodules.widgets.bootstrap.Modal

case class SelectedElements(selected: List[ObjectId], unselected: List[ObjectId])

abstract class StructuredMetaSnippet[T <: MongoIdRecord[T]](metaRecord: StructuredDbObject[T]) {

  def dbObjBridge = metaRecord

//  def chooseItem(curRecipients: List[ObjectId], onSel: Any => JsCmd): JsCmd = {
//    new Members().chooseItem(curRecipients, onSel)
//  }


}

abstract class StructuredFormSnippet[T <: MongoIdRecord[T]](val metaSnippet: StructuredMetaSnippet[T],
                                                            val modalScreen: StructuredLiftScreen[T],
                                                            val detailView: StructuredLiftScreen[T])
                                                           (implicit m: Manifest[T])
         extends DispatchSnippet with Observing with SnippetHelpers {

  case class SnippetFunctions(isMulti: Boolean, rawOnSel: Any => JsCmd, selFunc: T => JsCmd)

  /**
   * A paginator definition that will be used for returning paged results,
   * filtering and sorting the data
   */
  def listPaginator: Paginator[T]

  modalScreen.snippetInstance.set(Full(this))
  detailView.snippetInstance.set(Full(this))

  onSnippetInit()

  /**
   * Called when the snippet is loaded, before any content is shown.
   */
  def onSnippetInit(): Unit = {

    /**
     * Check for a filter param to pre-filter the result set
     */
    for (filter <- S.param("filter")) {
      listPaginator.setSearchQuery(filter)
      S.appendJs(Run("$('.search-query', '[name=master-table-search]').val('%s')".format(filter)))
    }

  }

  def dispatch: DispatchIt = overridenDispatch orElse {
    case "masterView" => masterView
    case "list" => list
    case "createNew" => createNew
    case "detailView" => createDetailView
    case "paginate" => listPaginator.paginate
    case "sortColumns" => listPaginator.sortColumns
    case "itemsShown" => itemsShown
    case "search" => searchElements
  }

  def overridenDispatch: DispatchIt = {
    case "dummy" => list
  }

  implicit def modelToVarModel(model: T): Var[T] = Var(model)
  implicit def wrapInVar(list: List[T]): List[Var[T]] = list.map(a => Var(a))

  lazy val myModelObjs = BufferSignal[Var[T]](wrapInVar(initModelObjs) :_*)

  def setModelObjs(objs: List[T]) = {
    myModelObjs.value.clear
    myModelObjs.value ++= objs
  }

  def addOrUpdateModelObj(model: T) = {
    //println("UPDATE MODEL: "+model.getClass().getName+ " in "+this.getClass().getName)
    try {
      myModelObjs.value.find(_.value.id.get == model.id.get) match {
        case Some(oldObj) => oldObj.update(model)
        case _ => myModelObjs.value += model
      }
    } catch {
      case e: Exception => e.printStackTrace()
    }
  }

  def removeObjectFromModel(model: T) = {
    try {
      for (v <- myModelObjs.value.find(_.value.id.get == model.id.get)) {
        myModelObjs.value -= v
      }
    } catch {
      case e: Exception => e.printStackTrace()
    }
  }

  def initModelObjs(implicit m: Manifest[T]): List[T] = listPaginator.page.toList

  def removeObjectFromDB(obj: T): Boolean = obj.delete_!

  /**
   * create an ajaxInvoke bindable that fires a ConfirmDialog and removes the object if approved.
   */
  def ajaxRemoveAction(obj: T, title: =>String, msg: =>String): GUIDJsExp = {
    def doRemoveObj(): JsCmd = {
      if (removeObjectFromDB(obj)) { removeObjectFromModel(obj) }; ajaxRemoveActionJsCmd(obj)
    }
    ajaxInvoke(()=>{ Bs3ConfirmRemoveDialog(title, msg, doRemoveObj _) })
  }

  def ajaxRemoveActionJsCmd(obj: T): JsCmd = myDetailView(obj).unselectIfObj(obj)

  def saveDataHolder(data: T): Unit = data.save(true)

  def myModalScreen: StructuredLiftScreen[T] = myModalScreen(modalScreen.screenVar.is)
  def myDetailView: StructuredLiftScreen[T] = myDetailView(detailView.screenVar.is)

  def myModalScreen(data: T): StructuredLiftScreen[T] = modalScreen
  def myDetailView(data: T): StructuredLiftScreen[T] = detailView

  def listElement(user: User, selFunc: T=>JsCmd)(element: T): CssSel

  def bindEdit(sel: String, obj: T): CssSel = {
     val modalScreen = myModalScreen(obj)
     withPerm(sel, modalScreen.objEditPermissions(obj) :_*)(modalScreen.setAndRenderModal(obj))
  }

  def doBindRemoveAction(obj: T): GUIDJsExp

  def bindRemove(sel: String, obj: T): CssSel = {
     withPerm(sel, myModalScreen(obj).objDeletePermissions(obj) :_*)(doBindRemoveAction(obj))
  }

  def createNew = {
    withPerm("@new-screen", myModalScreen.objCreatePermissions(myModalScreen.screenVar.is) :_*){ ajaxInvoke(()=>myModalScreen.createDialog) } &
    withElsePerm(myModalScreen.objCreatePermissions(myModalScreen.screenVar.is) :_*)(clientCreateNew)(notExistent)
  }

  /**
   * Called when the markup for adding a new record is bound.
   */
  def clientCreateNew = notExistent

  /**
   * Called when a list of elements is created. This is a hook to add global binding,
   * not per one record, but for the whole list.
   */
  def clientList(multi: Boolean, onSel: Any=>JsCmd) = {
    "@select-elems-submit [onclick]" #> ajaxInvoke(()=>{
      onSel(SelectedElements(selectedElems.toList, removedElems.toList))
    })
  }

  def createDetailView(in: NodeSeq): NodeSeq = this.myDetailView.toForm

  /**
   * Select the detail view for a given record
   * @param collapseTable do collapse the master overview table if true
   * @param record the record whose details are so very important
   * @return a JsCmd that manipulates the client view.
   */
  def selDetailView(collapseTable: Boolean)(record: T): JsCmd = {
    myDetailView(record).setAndRenderView(record) &
    (if (collapseTable) OnLoad(Run("$('#master-table-collapse').collapse('hide'); $('#detail-view-collapse').collapse('show')")) else Noop)
  }

  def chooseItem(multi: Boolean, curRecipients: List[ObjectId], onSel: Any=>JsCmd, tplName: String): JsCmd = {
    selectedElems ++= curRecipients
    def formTemplate(): NodeSeq = SnippetHelpers.formPartTpl(tplName)
    val html = renderChooseItem(multi, onSel)(formTemplate)
    Modal(html, "class" -> "max members-items-chooser", "backdrop" -> false, "keyboard" -> false)
  }

  def selChosenItem(multi: Boolean, onSel: Any=>JsCmd)(record: T): JsCmd = ajaxInvoke{ ()=>
    val b = toggleElementSelected(record.id.get)
    record.setSelected(b)
    if (multi) {
      if (b) {
        Run("$('#chk-%s').attr('checked', 'checked');".format(record.id.get.toString))
      } else {
        Run("$('#chk-%s').removeAttr('checked');".format(record.id.get.toString))
      }
    } else {
      onSel(SelectedElements(selectedElems.toList, removedElems.toList)) &
      Run("$('.members-items-chooser.modal').modal('hide')")
    }
  }

  def detailViewSelectByParamOrFirst(): JsCmd = {
    (for {
      select <- S.param("select")
      record <- myModelObjs.value.find(_.value.id.get.toString == select)
    } yield {
      OnLoad(selDetailView(true)(record.value))
    }) openOr detailViewSelectFirstItem()
  }

  def detailViewSelectFirstItem(): JsCmd = {
    (for (first <- myModelObjs.value.headOption) yield {
      OnLoad(selDetailView(false)(first.value))
    }) getOrElse Noop
  }

  def masterView: NodeSeq => NodeSeq = masterView(SnippetFunctions(false, l=>Noop, selDetailView(true)))
  def renderChooseItem(multi: Boolean, onSel: Any=>JsCmd): NodeSeq => NodeSeq = masterView(SnippetFunctions(multi, onSel, selChosenItem(multi, onSel)))

  def masterView(func: SnippetFunctions): NodeSeq => NodeSeq = {
    clientList(func.isMulti, func.rawOnSel) andThen
    "@master-table-header *" #> listPaginator.masterTableFields.filter(_.header).map(h => buildTableHeadElement(h)) &
    "@master-table-column" #> listPaginator.masterTableFields.filter(_.header).map(h => buildTableDataElement(h)) &
    "@master-table-items-shown" #> itemsShown &
    "@master-table-pagination" #> {ns: NodeSeq => listPaginator.paginate(ns)} &
    "@master-table-search" #> searchElements andThen
    "@master-table-sortable" #> {ns: NodeSeq => listPaginator.sortColumns(ns)} &
    "@master-table-body" #> list(func.selFunc) andThen
    "#table-definition *+" #> Script(detailViewSelectByParamOrFirst)
  }

  def buildTableHeadElement(header: DbField) = {
    // new UnprefixedAttribute("class", "span1", scala.xml.Null)
    "@master-table-th" #> <span name={"sort_"+header.field.name}><span>{header.displayName}&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;</span></span>
  }

  def buildTableDataElement(header: DbField) = {
    "@master-table-column [name]" #> header.field.name
  }

  def list: CssSel = list(rec => Noop)

  def list(selFunc: T => JsCmd): CssSel = {
    (for (user <- User.currentUser) yield {
      "#tbl-model-list [id]" #> nextFuncName &
      "#tbl-model-list" #> Repeater {
        myModelObjs.now map { modelVar =>
          try {
            "@eachModelObj" #> Cell(modelVar.map(listElement(user, selFunc)))
          } catch {
            case e: Exception => e.printStackTrace()
            notExistent
          }
        } signal
      }
    }) openOr notExistent
  }

  def listRecordElements(user: User, record: T, selFunc: T=>JsCmd)(implicit m: Manifest[T]): CssSel = {
    def listRecordElement(fieldDef: DbField) = {
      ("@%s *".format(fieldDef.field.name)) #> fieldDef.fieldValue(record)(m) &
      ("@%s [onclick]".format(fieldDef.field.name)) #> selFunc(record)
    }

    bindSelectDeselectElement(record) &
    listPaginator.masterTableFields.filter(_.header).map(listRecordElement).foldLeft(notExistent)(_ & _)
  }

  def bindSelectDeselectElement(record: T)(implicit m: Manifest[T]): CssSel = {
    "@select-elem-td *" #> ajaxCheckbox(selectedElems.contains(record.id.get),
                           (b: Boolean) => {modifySelected(record.id.get, b); Noop},
                           "style" -> "vertical-align:top",
                           "id" -> ("chk-"+record.id.get))
  }

//  def editRecord(record: T): CssSel = {
//    editRecordElement("@edit", record)
//  }
//
//  def editRecordElement(sel: String, record: T): CssSel = {
//    "%s [onclick]".format(sel) #> myLiftScreen.setAndRenderJs(record)
//  }

  val itemsSelect = List(("10", "10"), ("20", "20"), ("50", "50"), ("100", "100"))
  var _itemsPerPage = tryo(itemsSelect(1)._1.toInt) openOr 10

  def itemsShown = {
    "@itemsPerPage" #> SHtml.ajaxSelect(itemsSelect, Full(itemsSelect(1)._2), s => {
//      println("Value: "+s)
      _itemsPerPage = tryo(s.toInt) openOr 10
      listPaginator.setPaginationStart
      listPaginator.reloadModelData
      listPaginator.rerender
    })
  }

  def searchElements = {
    def process(query: String): JsCmd = {
      listPaginator.setSearchQuery(query)
      listPaginator.setPaginationStart
      listPaginator.reloadModelData
      listPaginator.rerender
    }

    "@query" #> SHtml.onSubmit(process)
  }

  // Selecting, deselecting elements
  val selectedElems = collection.mutable.Set[ObjectId]()
  val removedElems  = collection.mutable.Set[ObjectId]()

  protected def modifySelected(id: ObjectId, b: Boolean): Boolean = {
    if (b) {
      selectedElems += id
      removedElems -= id
      b
    } else {
      selectedElems -= id
      removedElems += id
      b
    }
  }

  protected def toggleElementSelected(id: ObjectId): Boolean = {
    val b = selectedElems.contains(id)
    modifySelected(id, !b)
  }



}
