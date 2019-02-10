


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

trait BsStringTypedField extends StringTypedField with SnippetHelper {
  def isAutoFocus: Boolean = false

  private def elem = {
    val fieldId: String = uniqueFieldId.openOr(randomString(12))

    S.fmapFunc(SFuncHolder(this.setFromAny(_))) {
      funcName =>
      <input type={formInputType} maxlength={maxLength.toString}
        id={fieldId}
        name={funcName}
        value={valueBox openOr ""}
        tabindex={tabIndex.toString}
        class="form-control" /> % autofocus(isAutoFocus)
    }
  }

  override def toForm: Box[NodeSeq] = Full(elem)

  override def asHtml = valueBox.map(Text(_)) openOr NodeSeq.Empty

}

class BsStringField[OwnerType <: Record[OwnerType]](rec: OwnerType, maxLength: Int)
  extends StringField(rec, maxLength) with BsStringTypedField

class OptionalBsStringField[OwnerType <: Record[OwnerType]](rec: OwnerType, maxLength: Int)
  extends OptionalStringField(rec, maxLength) with BsStringTypedField
