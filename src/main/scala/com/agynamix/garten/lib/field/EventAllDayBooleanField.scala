package com.agynamix.garten.lib.field

import net.liftweb.record.Record
import net.liftweb.record.field.BooleanField
import net.liftweb.common._
import net.liftweb.util.Helpers._
import net.liftweb.http.SHtml
import scala.xml.NodeSeq

abstract class EventAllDayBooleanField[OwnerType <: Record[OwnerType]](rec: OwnerType) extends BooleanField(rec) {

  private def elem(attrs: SHtml.ElemAttr*) = {
    val fieldId: String = uniqueFieldId.openOr(randomString(12))

    <div class="checkbox">{
    SHtml.checkbox(valueBox openOr false, (b: Boolean) => this.setBox(Full(b)),
          (("tabindex" -> tabIndex.toString): SHtml.ElemAttr) ::
          (("id" -> fieldId): SHtml.ElemAttr) ::
          (("onclick" -> "jQuery(document).trigger('event-allday-toggle', $(this))"): SHtml.ElemAttr) ::
          attrs.toList: _*)
    }</div>
  }

  override def toForm: Box[NodeSeq] =Full(elem())


}