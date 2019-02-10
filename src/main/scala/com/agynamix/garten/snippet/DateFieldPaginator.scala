package com.agynamix.garten.snippet

import net.liftweb.mongodb.record.MongoRecord
import net.liftweb.mongodb.BsonDSL._
import net.liftweb.mongodb.Limit
import net.liftweb.mongodb.Skip
import net.liftweb.json._
import net.liftweb.json.JsonDSL
import net.liftweb.mongodb.record.MongoMetaRecord
import com.agynamix.garten.model.User
import com.agynamix.garten.lib.StructuredDbObject
import com.agynamix.garten.model.share.MongoIdRecord
import net.liftweb.http.js.JsCmds._
import net.liftweb.http.js.JsCmd
import scala.xml.NodeSeq
import net.liftweb.http.SHtml._
import net.liftweb.util.BaseField
import java.util.Date
import scala.xml.Text
import net.liftweb.http.S
import net.liftweb.http.SHtml
import scala.xml.Unparsed
import net.liftweb.util.CssSel
import net.liftweb.util.Helpers._
import net.liftweb.common._
import org.joda.time.DateTime
import java.text.SimpleDateFormat
import com.mongodb.QueryBuilder
import org.joda.time.format.ISODateTimeFormat
import com.mongodb.BasicDBObject
import com.foursquare.rogue.LiftRogue._
import com.agynamix.garten.model.Event
import net.liftweb.mongodb.FindOption
import com.mongodb.DBObject
import net.liftweb.mongodb.JObjectParser

abstract class DateFieldPaginator[T <: MongoIdRecord[T]](snippet: StructuredFormSnippet[T], dateFieldName: String)
                                                        (implicit m: Manifest[T])
               extends BaseGartenPaginator[T](snippet) with AjaxSortedPaginator[T] with AjaxDateFieldPaginator[T] {

  override def itemsPerPage = Int.MaxValue

  override def rerender: JsCmd =
    super[AjaxSortedPaginator].rerender & super[AjaxDateFieldPaginator].rerender & paginatorJs

  override def page = {
    val field = headers(sort._1)._2
    val dbResult = (for (user <- User.currentUser; itsKeyFields <- keyFields(user)) yield {
      findPaginatedSortedByUser(user, itsKeyFields(), startDate, endDate, field, sort._2)
    }) openOr Nil
    filterDbPaginationResult(dbResult)
  }

  def findPaginatedSortedByUser(user: User, _keyFields: JObject, start: DateTime, end: DateTime, sortField: String, asc: Boolean): List[T] = {

    implicit val formats = DefaultFormats

    val sortOrder = if (asc) 1 else -1

    val reverseSort:Int = if (reverse(sortField)) -1 else 1
    val searchParams = (for (query <- searchQuery) yield {

      def regSearch(field: DbField) = "/.*%s.*/i.test(this.%s)".format(query, field.field.name)
      val filterFields = masterTableFields.filter(_.filter).map(field => ("$where" -> regSearch(field)))

      _keyFields ~ ("$or" -> filterFields)
    }) openOr _keyFields

    val dateRange = QueryBuilder.start().put("startDate").greaterThanEquals(start.toDate()).lessThanEquals(end.toDate()).get

//    val query3 = new BasicDBObject(dateFieldName, new BasicDBObject("$gte", start.toDate()).append("$lt", end.toDate()));

    val sort = new BasicDBObject(sortField, reverseSort * sortOrder)
    //println("Sort: "+sort)

    val all = JObjectParser.parse(searchParams)
    all.putAll(dateRange: DBObject)

    dbObjBridge.findAll( all, sort )
  }

}
/**
 * A paginator that uses an offset and a limit per page to
 * paginate through existing items.
 * In most cases this is the correct paginator to use.
 * Other cases might involve paginating by month, for instance.
 */
trait AjaxDateFieldPaginator[T] extends Paginator[T] {

  /**
   * Paginator interface
   */
  def setPaginationStart: Unit = {
    startDate = getPaginationStart
//    println("startDate: "+startDate)
  }

  def getPaginationStart: DateTime = {
    val re = new DateTime().withDayOfMonth(1).toDateMidnight().toDateTime()
//    println("startDate: "+re)
    re
  }

  def endDate: DateTime = {
    val re = new DateTime(startDate).plusMonths(1).withDayOfMonth(1).toDateMidnight().toDateTime()
//    println("endDate: "+re)
    re
  }

  /**
   * Calculates the number of pages the items will be spread across
   */
  //def numPages = 12

  /**
   * Calculates the current page number, based on the value of 'first.'
   * get current displayed month and find 0 based page index
   */
//  def curPage: Int = {
//    val re = startDate.getMonthOfYear()
//    println("Month of year: "+re)
//    re
//  }

  val fmt = new SimpleDateFormat("dd.MM.yyyy")

  /**
   * How to display the page's starting record
   */
  def recordsFrom: String = fmt.format(startDate.toDate())
  /**
   * How to display the page's ending record
   */
  def recordsTo: String = fmt.format(endDate.toDate())
  /**
   * The status displayed when using &lt;nav:records/&gt; in the template.
   */
  def currentXml: NodeSeq =
    if(count==0)
      Text(S.?("paginator.norecords"))
    else
      Text(S.?("paginator.displayingrecords",
           Array(recordsFrom, recordsTo, count).map(_.asInstanceOf[AnyRef]) : _*))

  /**
   * The template prefix for general navigation components
   */
  def navPrefix = "nav"

  var _startDate: DateTime = getPaginationStart

  /**
   * Sets the default starting record of the page (URL query parameters take precedence over this)
   */
  def startDate_=(d: DateTime) = _startDate = d

  def startDate = _startDate

  def pageXml(newStartDate: DateTime, ns: NodeSeq): NodeSeq = {
    if(startDate == newStartDate)
      // In case of current page we show the active style
      <a href="JavaScript://">{ns}</a>
    else
      <a href="JavaScript://" onclick={loadNextPageCmd(newStartDate).toJsCmd}>{ns}</a>
  }

  def getPreviousMonth(date: DateTime): DateTime = {
    val re = date.withDayOfMonth(1).minusMonths(1).toDateMidnight().toDateTime()
    println("Previous Month: "+re)
    re
  }

  def getNextMonth(date: DateTime): DateTime = {
    val re = date.withDayOfMonth(1).plusMonths(1).toDateMidnight().toDateTime()
    println("Next Month: "+re)
    re
  }

  private lazy val pagMemo = SHtml.idMemoize(ignored => paginateIt _)

  def rerender = pagMemo.setHtml()

  def paginate(ns: NodeSeq): NodeSeq = pagMemo(ns)

  def paginateIt(xhtml: NodeSeq) = {
    val css =
         "@first" #> "" &
         "@prev" #> pageXml(getPreviousMonth(startDate), prevXml) &
         "@allpages" #> "" &
         "@next" #> pageXml(getNextMonth(startDate), nextXml) &
         "@last" #> "" &
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


  def loadNextPageCmd(newStartDate: DateTime): JsCmd = {
    ajaxInvoke(()=>{
      startDate = newStartDate
      reloadModelData
      rerender
    })._2.cmd
  }

}
