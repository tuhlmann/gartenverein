package com.agynamix.garten.lib.field

import scala.xml._
import net.liftweb.util._
import net.liftweb.common._
import net.liftweb.http.S
import net.liftweb.json._
import net.liftweb.record._
import net.liftweb.record.field._
import S._
import Helpers._
import net.liftmodules.extras.SnippetHelper
import scala.xml.Text
import scala.xml.NodeSeq
import eu.henkelmann.actuarius.{Decorator, Transformer}
import eu.henkelmann.actuarius.ActuariusTransformer

trait BsSimpleEditorTextareaTypedField extends TextareaTypedField with SnippetHelper {
  lazy val transformer = new ActuariusTransformer
  
  def isAutoFocus: Boolean = false

  lazy val fieldIdPref = nextFuncName
  lazy val calcFieldId = uniqueFieldId.openOr(randomString(12))

  override def uniqueFieldId: Box[String] = Full(fieldIdPref + "_" + super.uniqueFieldId.openOr(""))

def tryMarkdownConversion(html: String): NodeSeq = {
    tryo(XML.loadString("<div>%s</div>".format(transformer(html)))).openOr(Text(html))
  }
  
  override def asHtml = {
    val v = valueBox.openOr("")
    val re = tryMarkdownConversion(v)
    println("MarkDown: "+re)
    re
  }

  private def elem = {
    S.fmapFunc(SFuncHolder(this.setFromAny(_))){
    funcName => <textarea class="form-control" id={calcFieldId} name={funcName}
      rows={textareaRows.toString}
      cols={textareaCols.toString}
      tabindex={tabIndex.toString}>{asHtml}</textarea> % autofocus(isAutoFocus)
    }
  }

  override def toForm: Box[NodeSeq] = Full(elem)

}

class BsSimpleEditorTextareaField[OwnerType <: Record[OwnerType]](rec: OwnerType, maxLength: Int)
  extends StringField(rec, maxLength) with BsSimpleEditorTextareaTypedField

class OptionalBsSimpleEditorTextareaField[OwnerType <: Record[OwnerType]](rec: OwnerType, maxLength: Int)
  extends OptionalStringField(rec, maxLength) with BsSimpleEditorTextareaTypedField
