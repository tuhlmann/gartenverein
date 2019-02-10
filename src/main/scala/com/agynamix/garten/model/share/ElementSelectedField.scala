package com.agynamix.garten.model.share

import net.liftweb.record.field.BooleanField
import net.liftweb.record.Record
import net.liftweb.http.SHtml
import net.liftweb.common._
import net.liftweb.util.Helpers._
import scala.xml.NodeSeq

abstract class ElementSelectedField[OwnerType <: Record[OwnerType]](_rec: OwnerType) extends BooleanField(_rec) {
  override def defaultValue = false
  override def ignoreField_? = true

  private def elem(attrs: SHtml.ElemAttr*) =
    <span>
      {SHtml.checkbox(valueBox openOr false, (b: Boolean) => this.setBox(Full(b)),
          (("tabindex" -> tabIndex.toString): SHtml.ElemAttr) :: (("style" -> "vertical-align:top"): SHtml.ElemAttr) :: attrs.toList: _*)}
    </span>

  override def toForm: Box[NodeSeq] =
    uniqueFieldId match {
      case Full(id) => Full(elem("id" -> id))
      case _ => Full(elem())
    }

}
