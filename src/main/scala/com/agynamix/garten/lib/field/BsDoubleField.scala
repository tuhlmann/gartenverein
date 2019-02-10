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

trait BsDoubleTypedField extends DoubleTypedField with SnippetHelper {
  def isAutoFocus: Boolean = false

  private def elem = {
    val fieldId: String = uniqueFieldId.openOr(randomString(12))

    S.fmapFunc(SFuncHolder(this.setFromAny(_))) {
      funcName =>
        <input type="number"
          id={fieldId}
          name={funcName}
          value={(valueBox openOr 0.0).toString}
          tabindex={tabIndex.toString}
          class="form-control" /> % autofocus(isAutoFocus)
    }
  }

  override def toForm: Box[NodeSeq] = Full(elem)
}

class BsDoubleField[OwnerType <: Record[OwnerType]](rec: OwnerType)
  extends DoubleField(rec) with BsDoubleTypedField

class OptionalBsDoubleField[OwnerType <: Record[OwnerType]](rec: OwnerType)
  extends OptionalDoubleField(rec) with BsDoubleTypedField
