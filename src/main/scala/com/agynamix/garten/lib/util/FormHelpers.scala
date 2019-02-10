package com.agynamix.garten.lib.util

import net.liftweb.common._
import net.liftweb.util.Helpers._
import net.liftweb.http.FileParamHolder
import net.liftweb.http.SHtml.ElemAttr
import scala.xml.Elem
import net.liftweb.http.S
import net.liftweb.http.S.BinFuncHolder
import net.liftweb.http.js.JsCmds._
import scala.xml.NodeSeq
import net.liftweb.http.FieldBinding
import net.liftweb.util.BaseField
import net.liftweb.http.AbstractScreen
import net.liftweb.http.SHtml
import net.liftweb.util.FieldError
import net.liftweb.mongodb.record.field.BsonRecordListField

trait FormHelpers {

//  self: AbstractScreen =>
//
//  def fieldWithUnderlying[T](underlying: => BsonRecordListField[_, UploadedDocument], defaultValue: => T,
//                                   theToForm: (Field { type ValueType = T } =>
//                                   Box[NodeSeq]),
//                                   stuff: FilterOrValidate[T]*): Field { type ValueType = T } = {
//
//    val newBinding: Box[FieldBinding] = (stuff.collect { case AFieldBinding(i) => i }).headOption
//
//    val newHelp: Box[NodeSeq] = (stuff.collect { case Help(ns) => ns }).headOption
//
//    val newTransforms: List[BaseField => NodeSeq => NodeSeq] = stuff.collect({
//      case FieldTransform(func) => func
//    }).toList
//
//    val newShow: Box[() => Boolean] = (stuff.collect { case DisplayIf(func) => func }).headOption
//
//    new Field {
//      type ValueType = T
//
//      override def name: String = underlying.name
//
//      override def displayName = underlying.displayName
//
//      override implicit def manifest = buildIt[T]
//
//      override def default: T = defaultValue
//
//      /**
//       * What form elements are we going to add to this field?
//       */
//      override lazy val formElemAttrs: Seq[SHtml.ElemAttr] = grabParams(stuff)
//
////      override val setFilter: List[T => T] = stuff.flatMap {
////        case AFilter(f) => Full(f)
////        case _ => Empty
////      }.toList
//
//      override val validations = stuff.flatMap {
//        case AVal(v: (T => List[FieldError])) => List(v)
//        case _ => Nil
//      }.toList
//
//      override def binding = newBinding
//
//      override def helpAsHtml = newHelp
//
//      override def asHtml = underlying.asHtml
//
//      override def toForm: Box[NodeSeq] = theToForm(this)
//
//      override def transforms = newTransforms
//
//      override def show_? = newShow map (_()) openOr (super.show_?)
//    }
//
//
//  }


}