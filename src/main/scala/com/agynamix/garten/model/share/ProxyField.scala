package com.agynamix.garten.model.share

import net.liftweb.util.BaseField
import scala.xml.NodeSeq
import net.liftweb.common._
import net.liftweb.util.Helpers._
import net.liftweb.mongodb.record.BsonRecord
import net.liftweb.record.Field
import net.liftweb.record.Record
import com.agynamix.garten.model.{User, Membership, Address, Document}
import net.liftweb.record.TypedField
import net.liftweb.record.field.StringField
import net.liftweb.mongodb.record.field.StringRefField
import net.liftweb.mongodb.record.MongoRecord
import net.liftweb.record.field.EmailField
import org.bson.types.ObjectId
import com.agynamix.garten.lib.field.{BsObjectIdRefField, PermissionListField, BsObjectIdRefListField}
import com.agynamix.garten.lib.Permission
import scala.xml.Text
import net.liftweb.http.S
import net.liftweb.http.S.SFuncHolder
import net.liftweb.http.SHtml
import com.agynamix.garten.config.Permissions
import net.liftweb.util.FieldError
import com.agynamix.garten.service.DocumentDownloadService
import net.liftweb.http.js.JsCmds.Run

/**
 * A trait for a value that appears to be on an object
 * but is injected from somewhere else.
 */
trait ReadOnlyProxyField[T] extends BaseField {
  type ValueType = T

  def value: ValueType

  def validate = Nil
  def validations = Nil
  def setFilter = Nil

  def set(in: ValueType): ValueType = { in }
  def get: ValueType = value
  def is = get
  def toForm: Box[NodeSeq] = Empty

}

/**
 * A proxy for a BsonList
 */
abstract class ProxyBsonField[OwnerType <: Record[OwnerType]](underlying: => StringField[Address]) extends BaseField {
  type ValueType = String

  def name = underlying.name

  override def uniqueFieldId = underlying.uniqueFieldId

  def validate = underlying.validate
  def validations = underlying.validations
  def setFilter = underlying.setFilter

  def set(in: ValueType): ValueType = {
    underlying.set(in)
    in
  }

  def get: ValueType = underlying.get
  def is = get
  def toForm: Box[NodeSeq] = underlying.toForm
  override def asHtml: NodeSeq = underlying.asHtml
  override def toString() = underlying.toString()
}

abstract class MembershipProxyField[OwnerType <: Record[OwnerType]](underlying: => Box[Membership]) extends BaseField {
  type ValueType = ObjectId

  def name = Membership.userRole.name

  def validate    = underlying.map(_.userRole.validate)    openOr Nil
  def validations = underlying.map(_.userRole.validations) openOr Nil
  def setFilter   = underlying.map(_.userRole.setFilter)   openOr Nil

  override def displayName = Membership.userRole.displayName

  override def set(in: ValueType): ValueType = { underlying.foreach(_.userRole.set(in)); in }

  override def get = underlying.map(_.userRole.get).openOr(ObjectId.get())
  def is = get

  override def toForm = underlying.flatMap(_.userRole.toForm)

  override def asHtml = underlying.map(_.userRole.asHtml) openOr NodeSeq.Empty

  override def toString() = underlying.toString()

}

abstract class ProxyDocumentURIField[OwnerType <: Record[OwnerType]](fieldName: String, underlying: => Box[Document]) extends BaseField {
  type ValueType = String

  def name = fieldName

  def validate    =  Nil
  def validations =  Nil
  def setFilter   =  Nil

  override def set(in: ValueType): ValueType = { in }

  override def get = underlying.map(_.downloadDocumentUrl()).openOr("")
  def is = get

  override def toForm = Empty

  override def asHtml = NodeSeq.Empty

  override def toString() = underlying.toString()

}


/**
 * A proxy for a String field
 */
abstract class ProxyStringField[OwnerType <: Record[OwnerType]](meta: => StringField[OwnerType], underlying: => Box[StringField[OwnerType]]) extends BaseField {
  type ValueType = String

  def name = meta.name

  def validate    = underlying.map(_.validate) openOr Nil
  def validations = underlying.map(_.validations) openOr Nil
  def setFilter   = underlying.map(_.setFilter) openOr Nil

  def set(in: ValueType): ValueType = { underlying.foreach(_.set(in)); in }

  def get: ValueType = underlying.map(_.get) openOr meta.defaultValue
  def is = get
  def toForm: Box[NodeSeq] = underlying.flatMap(_.toForm)
  override def asHtml: NodeSeq = {
    val re = underlying.map(_.asHtml) openOr NodeSeq.Empty
    //println("asHtml: "+re)
    re
  }
  override def toString() = underlying.map(_.toString()) openOr ""

}

/**
 * A proxy for an email field
 */
abstract class ProxyEmailField[OwnerType <: Record[OwnerType]](meta: => EmailField[OwnerType], underlying: => Box[EmailField[OwnerType]]) extends BaseField {
  type ValueType = String

  def name = meta.name

  def validate    = underlying.map(_.validate) openOr Nil
  def validations = underlying.map(_.validations) openOr Nil
  def setFilter   = underlying.map(_.setFilter) openOr Nil

  def set(in: ValueType): ValueType = { underlying.foreach(_.set(in)); in }

  def get: ValueType = underlying.map(_.get) openOr meta.defaultValue
  def is = get
  def toForm: Box[NodeSeq] = underlying.flatMap(_.toForm)
  override def asHtml: NodeSeq = {
    val re = underlying.map(_.asHtml) openOr NodeSeq.Empty
    //println("asHtml: "+re)
    re
  }
  override def toString() = underlying.map(_.toString()) openOr ""

}

/**
 * A proxy for an email field
 */
abstract class ProxyUserEmailField[OwnerType <: Record[OwnerType]](userRef: BsObjectIdRefField[_, User]) extends BaseField {
  type ValueType = Option[String]

  val meta = User.email
  def name = meta.name

  def validate    = Nil //underlying.map(_.validate) openOr Nil
  def validations = Nil //underlying.map(_.validations) openOr Nil
  def setFilter   = Nil //underlying.map(_.setFilter) openOr Nil

  def set(in: ValueType): ValueType = { in.foreach(v => userRef.obj.foreach(_.email.set(v))); in }

  def get: ValueType = userRef.obj match {
    case Full(user) if !user.email.isPlaceholderEmail => Full(user.email.get)
    case _ => Empty
  }
  def is = get
  def toForm: Box[NodeSeq] = userRef.obj match {
    case Full(user) if !user.email.isPlaceholderEmail => user.email.toForm
    case _ => Empty
  }

  override def asHtml: NodeSeq = userRef.obj match {
    case Full(user) if !user.email.isPlaceholderEmail => user.email.asHtml
    case _ => NodeSeq.Empty
  }

  override def toString() = userRef.obj match {
    case Full(user) if !user.email.isPlaceholderEmail => user.email.toString()
    case _ => ""
  }

}


/**
 * A proxy for selection of Permissions in a PermissionListField
 */
abstract class ProxyPermsOfRoleField[OwnerType <: BsonRecord[OwnerType]](underlying: => PermissionListField[OwnerType], permMatch: Permission) extends BaseField {
  type ValueType = List[Permission]

  val OPTION_NONE   = "none"
  val OPTION_RO     = "read_only"
  val OPTION_RW     = "read_write"
  val OPTION_RW_ALL = "read_write_all"

  val PERM_LABEL_PREFIX = "permission.select.name."

  def name = underlying.name + "_" + permMatch.domain

  override def displayName = S ? ("role.area.name." + permMatch.domain)

  def validate    = underlying.validate
  def validations = underlying.validations
  def setFilter   = underlying.setFilter

  def myPermissions = underlying.get.filter(p => p.implies(permMatch))

  def set(in: ValueType): ValueType = {
    val filtered = underlying.get.filterNot(p => p.implies(permMatch))
    underlying.set(filtered ::: in); in
  }

  def get: ValueType = myPermissions
  def is = get

  val options = List(OPTION_NONE, OPTION_RO, OPTION_RW, OPTION_RW_ALL)

  def localized(opt: String) = S ? (PERM_LABEL_PREFIX + opt)
  def buildDisplayList = options.map(o => (o, localized(o)))

  def hasReadWriteAll(perms: List[Permission]): Boolean = {
    perms.exists(p => p.implies(createReadWriteAllPerms.toSet))
  }

  def hasReadWrite(perms: List[Permission]): Boolean = {
    perms.exists(p => p.implies(createReadWritePerms.toSet))
  }

  def hasReadOnly(perms: List[Permission]): Boolean = {
    perms.exists(p => p.implies(createReadOnlyPerm))
  }

  def createReadOnlyPerm: Permission = Permission(permMatch.domain, Permissions.ACTION_RO)

  def createReadWritePerms: List[Permission] =
    Permission(permMatch.domain, Permissions.ACTION_CREATE) ::
    Permission(permMatch.domain, Permissions.ACTION_EDIT)   ::
    Permission(permMatch.domain, Permissions.ACTION_DELETE) :: Nil

  def createReadWriteAllPerms: List[Permission] =
    Permission(permMatch.domain, Permissions.ACTION_EDIT_ALL)   ::
    Permission(permMatch.domain, Permissions.ACTION_DELETE_ALL) :: Nil

  def getDefaultOption: String = {
    val myPerms = myPermissions
    if (hasReadWriteAll(myPerms)) OPTION_RW_ALL
    else if (hasReadWrite(myPerms)) OPTION_RW
    else if (hasReadOnly(myPerms)) OPTION_RO
    else OPTION_NONE
  }

  def setPermissions(opt: String): Unit = {
    opt match {
      case OPTION_RO     => this.set(List(createReadOnlyPerm))
      case OPTION_RW     => this.set(createReadOnlyPerm :: createReadWritePerms)
      case OPTION_RW_ALL => this.set((createReadOnlyPerm :: createReadWritePerms) ::: createReadWriteAllPerms)
      case _             => this.set(Nil)
    }

  }

  override def toForm: Box[NodeSeq] = {
    Full(SHtml.select(buildDisplayList, Full(getDefaultOption), setPermissions, "class" -> "form-control"))
  }

  override def asHtml: NodeSeq = {
    val re = Text(localized(getDefaultOption))
    //println("asHtml: "+re)
    re
  }
  override def toString() = underlying.toString()

}

