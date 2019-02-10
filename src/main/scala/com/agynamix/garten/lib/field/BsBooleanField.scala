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
import net.liftweb.http.SHtml

trait BsBooleanTypedField extends BooleanTypedField with SnippetHelper {
  def isAutoFocus: Boolean = false

  private def elem(attrs: SHtml.ElemAttr*) = {
    val fieldId: String = uniqueFieldId.openOr(randomString(12))

    <div class="checkbox">{
    SHtml.checkbox(valueBox openOr false, (b: Boolean) => this.setBox(Full(b)),
          (("tabindex" -> tabIndex.toString): SHtml.ElemAttr) ::
          (("id" -> fieldId): SHtml.ElemAttr) :: attrs.toList: _*)
    }</div>

  }

  override def toForm: Box[NodeSeq] = Full(elem())

  override def asHtml = Text((valueBox or defaultValueBox).map(boolToLocalized(_)).openOr(boolToLocalized(false)))

  private def boolToLocalized(b: Boolean) = S ? ("boolean.choice."+b.toString)

}

class BsBooleanField[OwnerType <: Record[OwnerType]](rec: OwnerType)
  extends BooleanField(rec) with BsBooleanTypedField

class OptionalBsBooleanField[OwnerType <: Record[OwnerType]](rec: OwnerType)
  extends OptionalBooleanField(rec) with BsBooleanTypedField
