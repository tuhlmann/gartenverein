package com.agynamix.garten.snippet

import net.liftweb.common._
import net.liftweb.util.Helpers._
import net.liftweb.util.BaseField
import net.liftweb.util.CssSel
import com.agynamix.garten.lib.util.SnippetHelpers
import com.agynamix.garten.model.share.MongoIdRecord

trait FieldTransforms[T <: MongoIdRecord[T]] extends StructuredLiftScreen[T] with SnippetHelpers {

  def adaptDocTblReadOnly()(field: BaseField): CssSel = {
    if (IsReadOnly) {
      ".action-td *" #> "" &
      ".fileinput-button [readonly]" #> "readonly" &
      ".fileinput-button [disabled]" #> "disabled" &
      ".fileinput-button [class+]" #> "disabled" &
      ".jq-fileupload-script" #> "" &
      ".jq-fileupload" #> ""
    } else {
      SnippetHelpers.notExistent
    }
  }


  def fullWidthField(field: BaseField): CssSel = {
    ".top-control-label" #> "" &
    ".top-control-body [class]" #> "control-body"
  }

  def removeIf(isRemove: => Boolean)(field: BaseField): CssSel = {
    if (isRemove) {
      ".fieldContainer" #> ""
    } else notExistent
  }

  def hideIf(isHidden: => Boolean)(field: BaseField): CssSel = {
    if (isHidden) {
      ".fieldContainer [style+]" #> "display:none"
    } else notExistent
  }

  def addContainerCls(cls: String)(field: BaseField): CssSel = {
    ".fieldContainer [class+]" #> cls
  }

  def htmlIf(isHtml: => Boolean, fieldId: String)(field: BaseField): CssSel = {
    //println("Field Name: "+fieldId+", "+IsReadOnly.is)
    if (isHtml) {
      ".controls *" #> <div class="panel panel-default serif no-border"><div class="panel-body">{field.asHtml}</div></div>
    } else {
      notExistent
    }
  }

  def hideDateIfAllDay(isAllDay: => Boolean)(field: BaseField): CssSel = {
    println("Date Field ID: "+field.uniqueFieldId)
    if (isAllDay) {
      ".date-input [style+]" #> "display:none"
    } else notExistent
  }

  def hideTimeIfAllDay(isAllDay: => Boolean)(field: BaseField): CssSel = {
    println("Date Field ID: "+field.uniqueFieldId)
    if (isAllDay) {
      ".time-input [style+]" #> "display:none"
    } else notExistent
  }


}