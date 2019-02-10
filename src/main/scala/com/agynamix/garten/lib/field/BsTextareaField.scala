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

trait BsTextareaTypedField extends TextareaTypedField with SnippetHelper {
  def isAutoFocus: Boolean = false

  private def elem = {
    val fieldId: String = uniqueFieldId.openOr(randomString(12))

    S.fmapFunc(SFuncHolder(this.setFromAny(_))){
      funcName =>
        <textarea
          id={fieldId}
          name={funcName}
          rows={textareaRows.toString}
          cols={textareaCols.toString}
          tabindex={tabIndex.toString}
          class="form-control">{valueBox openOr ""}</textarea> % autofocus(isAutoFocus)
    }
  }

  def asShortHtml = {
    val s = valueBox openOr ""
    val shorty = if (s.length() > 80) { s.substring(77)+"..." } else s
    Text(shorty)
  }

  override def toForm: Box[NodeSeq] = Full(elem)
}

class BsTextareaField[OwnerType <: Record[OwnerType]](rec: OwnerType, maxLength: Int)
  extends StringField(rec, maxLength) with BsTextareaTypedField

class OptionalBsTextareaField[OwnerType <: Record[OwnerType]](rec: OwnerType, maxLength: Int)
  extends OptionalStringField(rec, maxLength) with BsTextareaTypedField
