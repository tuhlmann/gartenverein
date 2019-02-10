package com.agynamix.garten.lib.field

import scala.xml._

import net.liftweb._
import common._
import http.{S, SHtml}
import json._
import record._
import record.field._
import util._

import S._
import Helpers._

import net.liftmodules.extras.SnippetHelper

trait BsEnumNameTypedField[EnumType <: Enumeration] extends EnumNameTypedField[EnumType] with SnippetHelper {
  def isAutoFocus: Boolean = false

  private def elem = {
    val fieldId: String = uniqueFieldId.openOr(randomString(12))

    SHtml.selectObj[Box[EnumType#Value]](buildDisplayList, Full(valueBox), setBox(_)) % ("tabindex" -> tabIndex.toString) % ("id" -> fieldId) % ("class" -> "form-control")
  }

  override def toForm: Box[NodeSeq] = Full(elem)
}

class BsEnumNameField[OwnerType <: Record[OwnerType], EnumType <: Enumeration](rec: OwnerType, protected val enum: EnumType)(implicit m: Manifest[EnumType#Value])
  extends Field[EnumType#Value, OwnerType] with MandatoryTypedField[EnumType#Value] with BsEnumNameTypedField[EnumType]
{
  def this(rec: OwnerType, enum: EnumType, value: EnumType#Value)(implicit m: Manifest[EnumType#Value]) = {
    this(rec, enum)
    set(value)
  }

  def owner = rec
  protected val valueManifest = m
}

class OptionalBsEnumNameField[OwnerType <: Record[OwnerType], EnumType <: Enumeration](rec: OwnerType, protected val enum: EnumType)(implicit m: Manifest[EnumType#Value])
  extends Field[EnumType#Value, OwnerType] with OptionalTypedField[EnumType#Value] with BsEnumNameTypedField[EnumType]
{
  def this(rec: OwnerType, enum: EnumType, value: Box[EnumType#Value])(implicit m: Manifest[EnumType#Value]) = {
    this(rec, enum)
    setBox(value)
  }

  def owner = rec
  protected val valueManifest = m
}
