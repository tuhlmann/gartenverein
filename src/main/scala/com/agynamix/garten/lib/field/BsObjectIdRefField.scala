package com.agynamix.garten.lib.field

import scala.xml._
import net.liftweb._
import common._
import http.{S, SHtml}
import json._
import mongodb.record._
import mongodb.record.field._
import util._
import S._
import Helpers._
import org.bson.types.ObjectId

class BsObjectIdRefField[OwnerType <: BsonRecord[OwnerType], RefType <: MongoRecord[RefType]](
  rec: OwnerType, val refMeta: MongoMetaRecord[RefType]
) extends ObjectIdField[OwnerType](rec) with MongoRefField[RefType, ObjectId] {

  private def elem = {
    val fieldId: String = uniqueFieldId.openOr(randomString(12))

    SHtml.selectObj[Box[MyType]](
      buildDisplayList,
      Full(valueBox),
      setBox(_)
    ) % ("tabindex" -> tabIndex.toString) % ("id" -> fieldId) % ("class" -> "form-control")
  }

  override def toForm = if (options.length > 0) Full(elem) else Empty
}

class BsOptionalObjectIdRefField[OwnerType <: BsonRecord[OwnerType], RefType <: MongoRecord[RefType]](
  rec: OwnerType, val refMeta: MongoMetaRecord[RefType]
) extends ObjectIdField[OwnerType](rec) with MongoRefField[RefType, ObjectId] {

  override def optional_? = true

  private def elem = {
    val fieldId: String = uniqueFieldId.openOr(randomString(12))

    SHtml.selectObj[Box[MyType]](
      buildDisplayList,
      Full(valueBox),
      setBox(_)
    ) % ("tabindex" -> tabIndex.toString) % ("id" -> fieldId) % ("class" -> "form-control")
  }

  override def toForm = if (options.length > 0) Full(elem) else Empty
}


class BsObjectIdRefListField[OwnerType <: BsonRecord[OwnerType], RefType <: MongoRecord[RefType]](
  rec: OwnerType, val refMeta: MongoMetaRecord[RefType]
) extends MongoRefListField[OwnerType, RefType, ObjectId](rec) {

  private def elem = SHtml.multiSelectObj[ObjectId](
    options,
    value,
    set(_)
  ) % ("tabindex" -> tabIndex.toString) % ("id" -> fieldId) % ("class" -> "form-control")

  override def toForm = if (options.length > 0) Full(elem) else Empty
}

class BsOptionalObjectIdRefListField[OwnerType <: BsonRecord[OwnerType], RefType <: MongoRecord[RefType]](
  rec: OwnerType, val refMeta: MongoMetaRecord[RefType]
) extends MongoRefListField[OwnerType, RefType, ObjectId](rec) {

  override def optional_? = true

  private def elem = SHtml.multiSelectObj[ObjectId](
    options,
    value,
    set(_)
  ) % ("tabindex" -> tabIndex.toString) % ("id" -> fieldId) % ("class" -> "form-control")

  override def toForm = if (options.length > 0) Full(elem) else Empty
}
