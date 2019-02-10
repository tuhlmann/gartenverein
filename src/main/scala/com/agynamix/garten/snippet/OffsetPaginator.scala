package com.agynamix.garten.snippet

import net.liftweb.mongodb.record.MongoRecord
import net.liftweb.mongodb.BsonDSL._
import net.liftweb.mongodb.Limit
import net.liftweb.mongodb.Skip
import net.liftweb.json.JsonAST.JObject
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
import net.liftweb.common._

abstract class BaseGartenPaginator[T <: MongoIdRecord[T]](snippet: StructuredFormSnippet[T])(implicit m: Manifest[T])
               extends AjaxSortedPaginator[T] with Paginator[T] {

  def filterDbPaginationResult(dbResult: List[T]): List[T] = {
    dbResult.filter(obj => snippet.myModalScreen(obj).isPermittedToViewElement(obj))
  }

  def dbObjBridge = snippet.metaSnippet.dbObjBridge

  def paginatorJs = Noop

  def reloadModelData(): Unit = {
    val records = page.toList
	snippet.setModelObjs(records)
  }

  def keyFields(user: User, args: Any*) = dbObjBridge.keyFields(user, args :_*)

  override def count =
    (for (user <- User.currentUser; itsKeyFields <- keyFields(user)) yield {
      dbObjBridge.count(itsKeyFields())
    }) openOr 0L

  def reverse(sortField:String):Boolean = false

}


abstract class OffsetPaginator[T <: MongoIdRecord[T]](snippet: StructuredFormSnippet[T])(implicit m: Manifest[T])
               extends BaseGartenPaginator[T](snippet) with AjaxSortedPaginator[T] with AjaxOffsetPaginator[T] {

  override def itemsPerPage = snippet._itemsPerPage

  override def rerender: JsCmd =
    super[AjaxSortedPaginator].rerender & super[AjaxOffsetPaginator].rerender & paginatorJs

  override def page = {
    val field = headers(sort._1)._2
    val dbResult = (for (user <- User.currentUser; itsKeyFields <- keyFields(user)) yield {
      findPaginatedSortedByUser(user, itsKeyFields(), curPage*itemsPerPage, itemsPerPage, field, sort._2)
    }) openOr Nil
    filterDbPaginationResult(dbResult)
  }

  def findPaginatedSortedByUser(user: User, _keyFields: JObject, offset: Int, limit: Int, sortField: String, asc: Boolean): List[T] = {
    val sortOrder = if (asc) 1 else -1

    val reverseSort:Int = if (reverse(sortField)) -1 else 1
    val searchParams = (for (query <- searchQuery) yield {

      def regSearch(field: DbField) = "/.*%s.*/i.test(this.%s)".format(query, field.field.name)
      val filterFields = masterTableFields.filter(_.filter).map(field => ("$where" -> regSearch(field)))

      _keyFields ~ ("$or" -> filterFields)
    }) openOr _keyFields
    val re = dbObjBridge.findAll(searchParams , (sortField -> reverseSort * sortOrder), Limit(limit), Skip(offset))
    re
  }

}

/**
 * The OffsetPaginator cannot take into account the sorting and filtering of fields not in the current collection,
 * but shown through proxy fields. The InMemoryPaginator is an attempt to remedy that. It will
 * fetch all relevant records, then do sorting, filtering and pagination in Scala.
 */
abstract class InMemoryPaginator[T <: MongoIdRecord[T]](snippet: StructuredFormSnippet[T])(implicit m: Manifest[T])
               extends BaseGartenPaginator[T](snippet) with AjaxSortedPaginator[T] with AjaxOffsetPaginator[T] {

  override def itemsPerPage = snippet._itemsPerPage

  override def rerender: JsCmd =
    super[AjaxSortedPaginator].rerender & super[AjaxOffsetPaginator].rerender & paginatorJs

  override def page = {
    val field = headers(sort._1)._2
    val dbResult = (for (user <- User.currentUser; itsKeyFields <- keyFields(user)) yield {
      findPaginatedSortedByUser(user, itsKeyFields(), curPage*itemsPerPage, itemsPerPage, field, sort._2)
    }) openOr Nil
    filterDbPaginationResult(dbResult)
  }

//  def filterMatches(record: T, filterFields: List[DbField], query: String): Boolean = {
//    val ql = query.trim.toLowerCase()
//
//    filterFields.exists(filterField => {
//      filterField.fieldValue(record).toString().toLowerCase().indexOf(ql) > -1
//    })
//  }

  def findPaginatedSortedByUser(user: User, _keyFields: JObject, offset: Int, limit: Int, sortField: String, asc: Boolean): List[T] = {
    val sortOrder = if (asc) 1 else -1
    val reverseSort:Int = if (reverse(sortField)) -1 else 1

    def compareByType(f1: BaseField, f2: BaseField): Boolean = {
      (f1.get, f2.get) match {
        case (v1: Int, v2: Int) => v1 < v2
        case (Some(v1: Int), Some(v2: Int)) => v1 < v2
        case (Some(v1: Date), Some(v2: Date)) => v1.before(v2)
        case (_ , Some(v2: Date)) => true
        case (Some(v1: Date), _)  => false
        case _ => f1.asHtml.toString() < f2.asHtml.toString()
      }
    }

    val re = dbObjBridge.findAll(_keyFields)

    // filter the collection
    val re2 = searchQuery match {
      case Full(query) if query.trim.nonEmpty =>
        val filterFields = masterTableFields.filter(_.filter) //.map(_.field.name)
        re.filter(rec => rec.filterMatches(filterFields, query))
      case _ => re
    }

    // Sort the remaining collection
    val re3 = re2.sortWith{ case (rec1, rec2) =>
      val comp = (for {f1 <- rec1.getField(sortField)
                       f2 <- rec2.getField(sortField)} yield {
        compareByType(f1, f2)
      }) getOrElse false
      if ((reverseSort*sortOrder) < 0) !comp else comp
    }

    // Skip and take
    re3.drop(offset).take(limit)
  }

}

