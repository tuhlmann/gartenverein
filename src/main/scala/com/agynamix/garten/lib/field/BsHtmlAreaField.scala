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
import com.agynamix.garten.lib.HtmlCleaner

trait BsHtmlAreaTypedField extends TextareaTypedField with SnippetHelper with Loggable {
  def isAutoFocus: Boolean = false

  lazy val fieldIdPref = nextFuncName
  lazy val calcFieldId = uniqueFieldId.openOr(randomString(12))

  override def uniqueFieldId: Box[String] = Full(fieldIdPref + "_" + super.uniqueFieldId.openOr(""))

  override def asHtml = asHtml(get.toString.trim)

  def asHtml(html: String) = {
    if (html.nonEmpty) {
      logger.debug("parse long: "+html)
      PCDataXmlParser(html).openOr(Text(html)) //MarkdownParser.parse(is).openOr(Text(is))
    } else {
      NodeSeq.Empty
    }
  }

  def shortHtml = {
    val txt = get.toString
    val len = txt.length() min 400
    val t = txt.substring(0, len) + (if (len < txt.length()) "..." else "")
    asHtml(t)
  }

  private def elem = {
    S.fmapFunc(SFuncHolder(s => this.setFromAny(HtmlCleaner.clean(s)))){
    funcName => <textarea class="form-control" id={calcFieldId} name={funcName}
      rows={textareaRows.toString}
      cols={textareaCols.toString}
      tabindex={tabIndex.toString}>{asHtml}</textarea> % autofocus(isAutoFocus)
    }
  }

  override def toForm: Box[NodeSeq] = Full(elem)

}

class BsHtmlAreaField[OwnerType <: Record[OwnerType]](rec: OwnerType, maxLength: Int)
  extends StringField(rec, maxLength) with BsHtmlAreaTypedField

class OptionalBsHtmlAreaField[OwnerType <: Record[OwnerType]](rec: OwnerType, maxLength: Int)
  extends OptionalStringField(rec, maxLength) with BsHtmlAreaTypedField
