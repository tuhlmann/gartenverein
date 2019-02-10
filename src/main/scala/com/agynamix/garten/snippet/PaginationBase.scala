package com.agynamix.garten.snippet

import scala.xml.NodeSeq
import net.liftweb.http.SHtml
import net.liftweb.common.Box
import net.liftweb.common.Empty
import net.liftweb.mongodb.record.MongoRecord
import net.liftweb.http.PaginatorSnippet
import java.util.regex.Pattern
import net.liftweb.util.CssSel
import scala.xml.Unparsed
import net.liftweb.http.SortedPaginator
import net.liftweb.util.Helpers._
import net.liftweb.http.SHtml._
import scala.xml.Text
import net.liftweb.http.js.JsCmd
import net.liftweb.common.Full
import net.liftweb.record.Field
import net.liftweb.record.Record
import net.liftweb.util.BaseField
import net.liftweb.http.js.JsCmds
//import net.liftweb.util.BindHelpers
import scala.xml.Null
import scala.xml.MetaData
import scala.xml.Elem
import com.agynamix.garten.lib.util.XmlHelpers
import com.agynamix.garten.model.share.MongoIdRecord
import net.liftweb.http.S
import scala.collection.mutable.ListBuffer
import com.agynamix.garten.lib.util.SnippetHelpers

class Def[C](implicit desired : Manifest[C]) extends DbFieldProp {
  val name = "typedef"

  def unapply[X](c : X)(implicit m : Manifest[X]) : Option[C] = {
    def sameArgs = desired.typeArguments.zip(m.typeArguments).forall {case (desired,actual) => desired >:> actual}
    if (desired >:> m && sameArgs) Some(c.asInstanceOf[C])
      else None
  }
}

trait DbFieldProp {
  def name: String
}
trait DbBoolProp extends DbFieldProp {
  def value: Boolean
}
trait DbStringProp extends DbFieldProp {
  def value: String
}
case class DbFieldHeader(val value: Boolean = true) extends DbBoolProp {
  val name = "header"
}
case class DbFieldSort  (val value: Boolean = true) extends DbBoolProp {
  val name = "sort"
}
case class DbFieldFilter(val value: Boolean = true) extends DbBoolProp {
  val name = "filter"
}
case class DbFieldDispName(val value: String) extends DbStringProp {
  val name = "displayName"
}

case class DbFieldExtract[T <: MongoRecord[T]](val extractFunc: (BaseField, T) => NodeSeq)(implicit m: Manifest[T]) extends DbFieldProp {
  val name = "extractor"
  def extract(field: BaseField, record: T) = extractFunc(field, record)
}

case class DbDefaultFieldExtract() extends DbFieldProp {
  val name = "extractor"
  def extract[T <: MongoIdRecord[T]](field: BaseField, record: T) = {
    record.allRecordFields.find(f => f.name == field.name).map(_.asHtml) getOrElse NodeSeq.Empty
  }
}

class DbField(val field: BaseField, val props: DbFieldProp*) {

  def getBoolProperty(name: String, defValue: Boolean) = {
    props.find(_.name == name).map {
      case v: DbBoolProp => v.value
      case _ => defValue
    } getOrElse defValue
  }

  def getStringProperty(name: String, defValue: String) = {
    props.find(_.name == name).map {
      case v: DbStringProp => v.value
      case _ => defValue
    } getOrElse defValue
  }

  def displayName = getStringProperty("displayName", field.displayName)
  def header      = getBoolProperty("header", true)
  def sort        = getBoolProperty("sort", true)
  def filter      = getBoolProperty("filter", true)
  def label       = getBoolProperty("label", true)

  def fieldValue[T <: MongoIdRecord[T]](record: T)(implicit itsMan: Manifest[T]): NodeSeq = {

    import shapeless.Typeable._

    props.find(_.name == "extractor").map{
      case e: DbDefaultFieldExtract => e.extract(field, record)
      case e: DbFieldExtract[_] => e.cast[DbFieldExtract[T]].map(_.extract(field, record)).getOrElse(NodeSeq.Empty)
      case _ => NodeSeq.Empty
    } getOrElse NodeSeq.Empty
  }

}

object DbField {

//  def apply(field: BaseField, props: DbFieldProp*) = new DbField(field, props :_*)

  def apply(field: BaseField, header: Boolean, sort: Boolean, filter: Boolean) =
    new DbField(field, DbFieldHeader(header), DbFieldSort(sort), DbFieldFilter(filter), DbDefaultFieldExtract())

  def apply[T <: MongoRecord[T]](field: BaseField, header: Boolean, sort: Boolean, filter: Boolean, extractFunc: (BaseField, T) => NodeSeq)(implicit m: Manifest[T]) = {
    val ft = new Def[DbFieldExtract[T]]
    new DbField(field, DbFieldHeader(header), DbFieldSort(sort), DbFieldFilter(filter), DbFieldExtract(extractFunc)(m))
  }

  def apply[T <: MongoRecord[T]](field: BaseField, dispName: String, header: Boolean, sort: Boolean, filter: Boolean, extractFunc: (BaseField, T) => NodeSeq)(implicit m: Manifest[T]) = {
    val ft = new Def[DbFieldExtract[T]]
    new DbField(field, DbFieldDispName(dispName), DbFieldHeader(header), DbFieldSort(sort), DbFieldFilter(filter), DbFieldExtract(extractFunc)(m))
  }

  def apply(field: BaseField, props: DbFieldProp*) = {
    val p = ListBuffer[DbFieldProp]()
    p += props.find(_.name == "displayName") getOrElse (DbFieldDispName(field.displayName))
    p += props.find(_.name == "header")      getOrElse (DbFieldHeader(true))
    p += props.find(_.name == "sort")        getOrElse (DbFieldSort(true))
    p += props.find(_.name == "filter")      getOrElse (DbFieldFilter(true))
    p += props.find(_.name == "extractor")   getOrElse (DbDefaultFieldExtract())
    new DbField(field, p :_*)
  }

}

/**
 * Common pagination functionality that is not tied to the way pagination
 * works.
 */
trait Paginator[T] {

  implicit val manifest: Manifest[T]

  /**
   * Reset the offset for the next pagination result
   */
  def setPaginationStart: Unit

  def paginate(ns: NodeSeq): NodeSeq

  /**
   * Define a set of fields for the master table rendering
   */
  def masterTableFields: List[DbField]

  def sortColumns(ns: NodeSeq): NodeSeq

  /**
   * Set a search query to filter the pagination result
   */
  def setSearchQuery(query: String): Unit

  /**
   * The total number of items
   */
  def count: Long

  /**
   * How many items to put on each page
   */
  def itemsPerPage = 20

  /**
   * The items displayed on the current page
   */
  def page: Seq[T]

//  /**
//   * Calculates the number of pages the items will be spread across
//   */
//  def numPages: Int
//
//  /**
//   * Calculates the current page number, based on the value of 'first.'
//   */
//  def curPage: Int

  /**
   * How to display the page's starting record
   */
  def recordsFrom: String
  /**
   * How to display the page's ending record
   */
  def recordsTo: String

  /**
   * The status displayed when using &lt;nav:records/&gt; in the template.
   */
  def currentXml: NodeSeq

  /**
   * A function that loads a new bunch of data using the page function and loads it so it will be shown.
   */
  def reloadModelData(): Any

  def rerender: JsCmd

  // Client side JS to attach to ours
  def paginatorJs: JsCmd

}

/**
 * A paginator that uses an offset and a limit per page to
 * paginate through existing items.
 * In most cases this is the correct paginator to use.
 * Other cases might involve paginating by month, for instance.
 */
trait AjaxOffsetPaginator[T] extends Paginator[T] {

  /**
   * Paginator interface
   */
  def setPaginationStart: Unit = first = 0L

  /**
   * Calculates the number of pages the items will be spread across
   */
  def numPages = (count/itemsPerPage).toInt + (if(count % itemsPerPage > 0) 1 else 0)

  /**
   * Calculates the current page number, based on the value of 'first.'
   */
  def curPage = (first / itemsPerPage).toInt

  /**
   * How to display the page's starting record
   */
  def recordsFrom: String = (first+1 min count) toString
  /**
   * How to display the page's ending record
   */
  def recordsTo: String = ((first+itemsPerPage) min count) toString
  /**
   * The status displayed when using &lt;nav:records/&gt; in the template.
   */
  def currentXml: NodeSeq =
    if(count==0)
      Text(S.?("paginator.norecords"))
    else
      Text(S.?("paginator.displayingrecords",
           Array(recordsFrom, recordsTo, count).map(_.asInstanceOf[AnyRef]) : _*))


  protected var _first = 0L
  /**
   * Sets the default starting record of the page (URL query parameters take precedence over this)
   */
  def first_=(f: Long) = _first = f max 0 min (count-1)

  def first = _first

  /**
   * Generates links to multiple pages with arbitrary XML delimiting them.
   */
  def pagesXml(pages: Seq[Int], sep: NodeSeq): NodeSeq =
    pages.toList map {n =>
      pageXml(n*itemsPerPage, Text(n+1 toString))
                    } match {
                      case one :: Nil => one
                      case first :: rest => rest.foldLeft(first) {
                        case (a,b) => a ++ sep ++ b
                      }
                      case Nil => Nil
                    }

  def pageXml(newFirst: Long, ns: NodeSeq): NodeSeq = {
    if(first==newFirst || newFirst >= count || newFirst < 0)
      // In case of current page we show the active style
      <a href="JavaScript://">{ns}</a>
    else
      <a href="JavaScript://" onclick={loadNextPageCmd(newFirst).toJsCmd}>{ns}</a>
  }

  private lazy val pagMemo = SHtml.idMemoize(ignored => paginateIt _)

  def rerender = pagMemo.setHtml()

  def paginate(ns: NodeSeq): NodeSeq = pagMemo(ns)

  def paginateIt(xhtml: NodeSeq)(implicit m: Manifest[T]) = {
    val css =
         elementState(0, "@prevElemState") &
         elementState(itemsPerPage*(numPages-1), "@nextElemState") &
         "@first" #> pageXml(0, firstXml) &
         "@prev" #> pageXml(first-itemsPerPage max 0, prevXml) &
         "@allpages" #> {(n:NodeSeq) => pagesXml(0 until numPages, n)} &
         "@next" #> pageXml(first+itemsPerPage min itemsPerPage*(numPages-1) max 0, nextXml) &
         "@last" #> pageXml(itemsPerPage*(numPages-1), lastXml) &
         "@records" #> currentXml &
         "@recordsFrom" #> Text(recordsFrom) &
         "@recordsTo" #> Text(recordsTo) &
         "@recordsCount" #> Text(count.toString)

    css(xhtml)
  }

  def firstXml = Unparsed("&laquo;")
  def lastXml  = Unparsed("&raquo;")
  def prevXml  = Unparsed(S ? "paginator.prev")
  def nextXml  = Unparsed(S ? "paginator.next")


  def elementState(newFirst: Long, selector: String): CssSel = {
    if(first==newFirst || newFirst >= count || newFirst < 0) {
      (selector + " [class+]") #> "disabled"
    } else {
      "#notExistent" #> ""
    }
  }

  def loadNextPageCmd(newFirst: Long)(implicit m: Manifest[T]): JsCmd = {
    ajaxInvoke(()=>{
      _first = newFirst
      reloadModelData
      rerender
    })._2.cmd
  }

}

trait AjaxSortedPaginator[T] extends Paginator[T] {

  /**
   * Pair of (column index, ascending)
   */
  type SortState = (Int, Boolean)

  protected var _sort = (0, true)

  /**
   * Get the current sort state: Pair of (column index, ascending?)
   */
  def sort: SortState = _sort

  /**
   * Set the current sort state: Pair of (column index, ascending?)
   */
  def sort_=(s: SortState) = _sort = s

  /**
   * Returns a new SortState based on a column index.
   * If the paginator is already sorted by that column, it
   * toggles the direction; otherwise the direction is ascending.
   * Note that this method does not alter the sort state in the
   * paginator; it only calculates the direction toggle.
   * Example usage:
   * sortedPaginator.sort = sortedPaginator.sortedBy(columns.indexOf(clickedColumn))
   */
  def sortedBy(column: Int): SortState = sort match {
    case (`column`, true) =>  // descending is only if it was already sorted ascending
      (column, false)
      case _ =>
        (column, true)
  }


  def sortPrefix = "sort"

  var _searchQuery: Box[String] = Empty
  def setSearchQuery(query: String) = _searchQuery = if (query == null || query.trim.isEmpty()) Empty else Full(query.trim())
  def searchQuery = _searchQuery

  def rerender = sortMemo.setHtml() // & super.rerender & paginatorJs
  private lazy val sortMemo = SHtml.idMemoize(ignored => _sortColumns _)

  def sortColumns(ns: NodeSeq) = sortMemo(ns)

  private def _sortColumns(html: NodeSeq): NodeSeq = {
    val css = sortCols.zipWithIndex.map {
      case ((binding, _), colIndex) =>
        s"@${sortPrefix}_${binding}" #> { ns: NodeSeq =>
          addAttributes2(sortColumnsMarkup(ns, colIndex), Null) //(BindHelpers.currentNode.map(_.attributes) openOr Null))
        }
    }.foldLeft(SnippetHelpers.notExistent)(_ & _)
    css(html)

//    val result = bind(sortPrefix, xhtml,
//      sortCols.zipWithIndex.map {
//        case ((binding, _), colIndex) =>
//          FuncBindParam(binding, ns =>
//            addAttributes2(sortColumnsMarkup(ns, colIndex), (BindHelpers.currentNode.map(_.attributes) openOr Null))
//          )
//      }.toSeq: _*)
//    result
  }

  /**
   * takes a NodeSeq and applies all the attributes to all the Elems at the top level of the
   * NodeSeq.  The id attribute is applied to the first-found Elem only
   */
  def addAttributes2(in: NodeSeq, attributes: MetaData): NodeSeq = {
    if (attributes == Null) {
      in
    } else {
      val noId = attributes.filter(_.key != "id")
      var doneId = false
      in map {
        case e: Elem =>
          if (doneId) e % noId else {
            doneId = true
            XmlHelpers.mergeAttributes(e, attributes)
          }
        case x => x
      }
    }
  }

  private def sortColumnsMarkup(ns: NodeSeq, colIndex: Int): NodeSeq = {
    def sortColumnsClass = {
      if (sort._1 == colIndex) {
        if (sort._2) "sorting_asc" else "sorting_desc"
      } else {
        "sorting"
      }
    }
    <a class={sortColumnsClass} href="Javascript://" onclick={ajaxInvoke(()=>doSortColumns(colIndex))._2.toJsCmd}>{ns}</a>
  }

  private def doSortColumns(colIndex: Int)(implicit m: Manifest[T]): JsCmd = {
    sort = sortedBy(colIndex)
    reloadModelData
    rerender
  }

  def masterTableFields: List[DbField]

  lazy val sortCols = masterTableFields.filter(_.sort).map(f => (f.field.name, f.field.name))
  lazy val headers  = masterTableFields.filter(_.header).map(f => (f.field.name, f.field.name))

}