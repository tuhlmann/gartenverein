package com.agynamix.garten.lib

import field._
import scala.xml.{NodeSeq, Text}
import org.bson.types.ObjectId
import net.liftweb._
import common._
import http.{CleanRequestVarOnSessionTransition, LiftResponse, RequestVar, S, SessionVar}
import mongodb.record._
import mongodb.record.field._
import record.MandatoryTypedField
import record.field.{PasswordField => _, _}
import net.liftweb.util.{Props, FieldError, Helpers}
import com.agynamix.garten.config.AuthUtil
import scalaz._
import Scalaz._
import net.liftweb.http.SHtml
import com.agynamix.garten.snippet.Roles
import com.agynamix.garten.snippet.SelectedElements
import net.liftweb.http.js.JsCmds
import com.agynamix.garten.model.share.OptionalObjectIdRefListField
import com.agynamix.garten.model.Role
import com.agynamix.garten.model.ExtSession

/**
 * AuthUser is a base class that gives you a "User" that has roles and permissions.
 */
trait AuthUser {
  /*
   * String representing the User ID
   */
  def userIdAsString: String

  /*
   * A list of this user's permissions
   */
  def authPermissions: Set[Permission]

  /*
   * A list of this user's roles
   */
  def authRoles: Set[Role]
}

trait AuthUserMeta[UserType <: AuthUser] {
  /*
   * True when the user request var is defined.
   */
  def isLoggedIn: Boolean
  /*
   * User logged in by supplying password. False if auto logged in by ExtSession or LoginToken.
   */
  def isAuthenticated: Boolean
  /*
   * Current user has the given role
   */
//  def hasRole(roleHandle: String): Boolean
//  def lacksRole(roleHandle: String): Boolean = !hasRole(roleHandle)
//  def hasAnyRoles(roleHandles: Seq[String]) = roleHandles exists (r => hasRole(r.trim))

  /*
   * Current user has the given permission
   */
  def hasPermission(permission: Permission): Boolean
  def lacksPermission(permission: Permission): Boolean = !hasPermission(permission)

  /*
   * Log the current user out
   */
  def logUserOut(): Unit

  /*
   * Handle a LoginToken. Called from Locs.loginTokenLocParams
   */
  def handleLoginToken(): Box[LiftResponse] = Empty

  /*
   * Handle a InvitationToken. Called from Locs.invitationTokenLocParams
   */
  def handleInvitationToken(): Box[LiftResponse] = Empty

}

/*
 * Trait that has login related code
 */
trait UserLifeCycle[UserType <: AuthUser] {

  /*
   * Given a String representing the User ID, find the user
   */
  def findByStringId(id: String): Box[UserType]

  // log in/out lifecycle callbacks
  def onLogIn: List[UserType => Unit] = Nil
  def onLogOut: List[Box[UserType] => Unit] = Nil

  // current userId stored in the session.
  private object curUserId extends SessionVar[Box[String]](Empty)
  def currentUserId: Box[String] = curUserId.is

  private object curUserIsAuthenticated extends SessionVar[Boolean](false)

  // Request var that holds the User instance
  private object curUser extends RequestVar[Box[UserType]](currentUserId.flatMap(findByStringId))
  with CleanRequestVarOnSessionTransition {
    override lazy val __nameSalt = Helpers.nextFuncName
  }
  def currentUser: Box[UserType] = curUser.is

  def isLoggedIn: Boolean = currentUserId.isDefined
  def isAuthenticated: Boolean = curUserIsAuthenticated.is

//  def hasRole(roleHandle: String): Boolean =
//    currentUser.map(_.authRoles.exists(_.roleHandle.is === roleHandle)).openOr(false)

  def hasPermission(permission: Permission): Boolean = {
    currentUser.map(u => {
      permission.implies(u.authPermissions)
    }).openOr(false)
  }

  def logUserIn(who: UserType, isAuthed: Boolean = false, isRemember: Boolean = false) {
    curUserId.remove()
    curUserIsAuthenticated.remove()
    curUser.remove()
    curUserId(Full(who.userIdAsString))
    curUserIsAuthenticated(isAuthed)
    curUser(Full(who))
    onLogIn.foreach(_(who))
    if (isRemember)
      ExtSession.createExtSession(who.userIdAsString)
  }

  def logUserOut() {
    onLogOut.foreach(_(currentUser))
    curUserId.remove()
    curUserIsAuthenticated.remove()
    curUser.remove()
    S.session.foreach(_.destroySession())
  }
}

/*
 * Mongo version of AuthUser
 */
trait MongoAuthUser[T <: MongoAuthUser[T]] extends MongoRecord[T] with AuthUser {
  self: T =>

  def id: MandatoryTypedField[_]
  def email: StringField[_]
}

/*
 * Mix this in for a simple user.
 */
trait ProtoAuthUser[T <: ProtoAuthUser[T]] extends MongoAuthUser[T] {
  self: T =>

  import Helpers._

//  object username extends StringField(this, 32) {
//    override def displayName = "Username"
//    override def setFilter = trim _ :: super.setFilter
//
//    private def valUnique(msg: => String)(value: String): List[FieldError] = {
//      if (value.length > 0)
//        meta.findAll(name, value).filterNot(_.id.is == owner.id.is).map(u =>
//          FieldError(this, Text(msg))
//        )
//      else
//        Nil
//    }
//
//    override def validations =
//      valUnique("Another user is already using that username, please enter a different one") _ ::
//      valMinLen(3, "Username must be at least 3 characters") _ ::
//      valMaxLen(32, "Username must be less than 33 characters") _ ::
//      super.validations
//  }

  object lastname extends BsStringField(this, 64) {
    override def displayName = "Nachname"

    override def validations =
      valMaxLen(64, "Nachname darf aus höchstens 64 Zeichen bestehen") _ ::
      valMinLen(2, "Bitte geben Sie einen Nachnamen an.") _ ::
      super.validations
  }

  object firstname extends BsStringField(this, 64) {
    override def displayName = "Vorname"

    override def validations =
      valMaxLen(64, "Vorname darf aus höchstens 64 Zeichen bestehen") _ ::
      valMinLen(2, "Bitte geben Sie einen Vornamen an.") _ ::
      super.validations
  }

  def fullname = firstname.get + " " + lastname.get

  /*
  * http://www.dominicsayers.com/isemail/
  */
  object email extends BsEmailField(this, 254) {
    override def displayName = "Email"
    override def setFilter = trim _ :: toLower _ :: super.setFilter

    private def valUnique(msg: => String)(value: String): List[FieldError] = {
      owner.meta.findAll(name, value).filter(_.id.get != owner.id.get).map(u =>
        FieldError(this, Text(msg))
      )
    }

    override def validations =
      valUnique("Diese Emailadresse ist bereits vergeben") _  ::
      valMaxLen(254, "Eine Emailadresse darf 254 Zeichen oder weniger belegen") _ ::
      valMinLen(2, "Bitte geben Sie eine Emailadresse an.") _ ::
      super.validations

    def isPlaceholderEmail: Boolean = {
      get.endsWith(Props.get("placeholder.email.domain", "example.com"))
    }

  }
  // email address has been verified by clicking on a LoginToken link
  object verified extends BooleanField(this) {
    override def displayName = "Verified"
  }
  object password extends BsPasswordField(this, 6, 32) {
    override def displayName = "Password"
  }
  object permissions extends PermissionListField(this)
  object roles extends OptionalObjectIdRefListField(this, Role) {
    override def displayName = "Rollen des Users"
    def permissions: List[Permission] = objs.flatMap(_.permissions.get)
//    def handles: List[String] = objs.map(_.roleHandle.is)
//    def localizedNames: List[String] = objs.map(_.roleLocalizedName.is)

    override def asHtml = Text(objs.map(_.displayName).mkString(", "))

    def optOptions: List[(String, String)] = {
      Role.findSystemRoles map (r => (r.id.get.toString, r.displayName))
    }

    override def toForm: Box[NodeSeq] = {
      val opt = optOptions
      //println("Role Options: "+opt)
      //println("Choosen: "+objs.map(_.id.get.toString))
      Full(SHtml.multiSelect(opt, objs.map(_.id.get.toString), v => {
        //println("Set this Role: "+v)
        val itsRoles: List[ObjectId] = (v.map(v => new ObjectId(v)) ::: this.valueBox.openOr(Nil)).distinct
        this.set(itsRoles)
      }, "class" -> "form-control"))
    }

  }

  //lazy val authPermissions: Set[Permission] = (permissions.is ::: roles.permissions).toSet
  //lazy val authRoles: Set[String] = roles.names.toSet Redefined in User

  lazy val fancyEmail = AuthUtil.fancyEmail(fullname, email.get)

  lazy val displayName = if (fullname.trim().nonEmpty) fullname else email.get

}

trait ProtoAuthUserMeta[UserType <: MongoAuthUser[UserType]]
extends MongoMetaRecord[UserType] with AuthUserMeta[UserType]
with UserLifeCycle[UserType] {
  self: UserType =>
}

