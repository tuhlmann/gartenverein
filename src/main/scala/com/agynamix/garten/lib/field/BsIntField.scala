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

trait BsIntTypedField extends IntTypedField with SnippetHelper {
  def isAutoFocus: Boolean = false

  private def elem = {
    val fieldId: String = uniqueFieldId.openOr(randomString(12))

    S.fmapFunc(SFuncHolder(this.setFromAny(_))) {
      funcName =>
        <input type={formInputType}
          id={fieldId}
          name={funcName}
          value={(valueBox openOr "").toString}
          tabindex={tabIndex.toString}
          class="form-control" /> % autofocus(isAutoFocus)
    }
  }

  override def asHtml = Text(valueBox.openOr("").toString)

  override def toForm: Box[NodeSeq] = Full(elem)
}

class BsIntField[OwnerType <: Record[OwnerType]](rec: OwnerType)
  extends IntField(rec) with BsIntTypedField

class OptionalBsIntField[OwnerType <: Record[OwnerType]](rec: OwnerType)
  extends OptionalIntField(rec) with BsIntTypedField
