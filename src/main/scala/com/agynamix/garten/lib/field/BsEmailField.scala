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

trait BsEmailTypedField extends EmailTypedField with SnippetHelper {
  def isAutoFocus: Boolean = false

  private def elem = {
    val fieldId: String = uniqueFieldId.openOr(randomString(12))

    S.fmapFunc(SFuncHolder(s => this.setFromString(s))) {
      funcName =>
      <input type={formInputType} maxlength="254"
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

class BsEmailField[OwnerType <: Record[OwnerType]](rec: OwnerType, maxLength: Int)
  extends EmailField(rec, maxLength) with BsEmailTypedField

class OptionalBsEmailField[OwnerType <: Record[OwnerType]](rec: OwnerType, maxLength: Int)
  extends OptionalEmailField(rec, maxLength) with BsEmailTypedField
