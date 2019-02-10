package com.agynamix.garten
package model

import com.agynamix.garten.lib.field.PermissionListField
import net.liftweb._
import mongodb.record._
import mongodb.record.field._
import record.field.StringField
import org.bson.types.ObjectId
import net.liftweb.http.S
import com.foursquare.rogue.LiftRogue._
import com.agynamix.garten.config.RolesDef
import com.agynamix.garten.config.Permissions
import com.agynamix.garten.model.share.MongoIdRecord
import com.agynamix.garten.model.share.ClientId
import com.agynamix.garten.model.share.CreatedUpdated
import com.agynamix.garten.lib.StructuredDbObject
import net.liftweb.record.field.BooleanField
import scala.xml.Text
import scala.xml.NodeSeq
import net.liftweb.mongodb.BsonDSL._
import net.liftweb.common._
import net.liftweb.json.JsonAST._
import net.liftweb.util.Helpers._
import net.liftweb.record.field.OptionalStringField
import net.liftweb.http.S.SFuncHolder
import com.agynamix.garten.lib.util.SnippetHelpers
import com.agynamix.garten.model.share.ProxyPermsOfRoleField
import com.agynamix.garten.lib.Permission
import net.liftweb.record.field.IntField
import com.agynamix.garten.lib.field.BsBooleanField
import com.agynamix.garten.lib.field.BsIntField
import com.agynamix.garten.lib.field.OptionalBsStringField
import com.agynamix.garten.lib.field.BsStringField


object Role extends Role with MongoMetaRecord[Role] with StructuredDbObject[Role] with SnippetHelpers {

  val DEFAULT_SORT_ORDER = 9999

  ensureIndex((roleHandle.name -> 1), false)

  def keyFields(user: User, args: Any*): Box[()=>JObject] =
    user.activeMembership.clientId.map(id => ()=>(clientId.name -> id))

  def createTeamInstance(clientId: ObjectId) = {
    Role.createRecord.clientId(clientId).category(RolesDef.CAT_TEAM)
  }

  def deleteRolesForClient(clientId: ObjectId): Unit = {
    (Role where (_.clientId eqs clientId) fetch) foreach (_.delete_!)
  }

  def allRoles(cat: String) = (Role where (_.category eqs cat) fetch)

  def createRole(handle: String, name: Box[String], category: String, sortOrder: Int, perms: Permission*): Role =
    createRecord.roleHandle(handle).roleLocalizedName(name).category(category).sortOrder(sortOrder).permissions(List(perms :_*))

  def createClientRole(clientId: ObjectId, handle: String, name: Box[String], category: String, perms: Permission*): Role =
    createRole(handle, name, category, DEFAULT_SORT_ORDER, perms :_*).clientId(clientId)

  def createDefaultRole(handle: String, name: Box[String], category: String, sortOrder: Int, perms: Permission*): Role =
    createRole(handle, name, category, sortOrder, perms :_*).defaultRole(true)

  def copyRole(role: Role): Role =
    createRole(role.roleHandle.get, localizeDisplayName(role.roleHandle.get).or(role.roleLocalizedName.get),
               role.category.get, role.sortOrder.get, role.permissions.get :_*)

  def copyClientRole(clientId: ObjectId, role: Role): Role = copyRole(role).clientId(clientId)

  def findDefaultRole(handle: String): Box[Role] =
    (Role where (_.roleHandle eqs handle) and (_.defaultRole eqs true) get())

  def findClientRole(clientId: ObjectId, handle: String): Box[Role] =
    (Role where (_.clientId eqs clientId) and (_.roleHandle eqs handle) and (_.defaultRole eqs false) get())

  def findClientRoles(clientId: ObjectId): List[Role] = Role where (_.clientId eqs clientId) fetch

  def findSystemRoles: List[Role] = Role where (_.defaultRole eqs true) and (_.category eqs RolesDef.CAT_SYSTEM) fetch
  def findDefaultTeamRoles: List[Role] = Role where (_.defaultRole eqs true) and (_.category eqs RolesDef.CAT_TEAM) fetch


  def findOrCreateDefault(handle: String, name: Box[String], category: String, sortOrder: Int, perms: Permission*): Role =
    findDefaultRole(handle) openOr createDefaultRole(handle, name, category, sortOrder, perms :_*)

  def findOrCreateAndSaveDefault(handle: String, category: String, sortOrder: Int, perms: Permission*): Role =
    findOrCreateDefault(handle, Empty, category, sortOrder, perms :_*).save(true)

  def findOrCreateAndSaveSU() = findOrCreateAndSaveDefault(RolesDef.R_SUPERUSER, RolesDef.CAT_SYSTEM, RolesDef.SORT_SUPERUSER, RolesDef.P_SUPERUSER.toSeq :_*)


  def findOrCreate(clientId: ObjectId, handle: String, name: Box[String], category: String, perms: Permission*): Role =
    findClientRole(clientId, handle) openOr createClientRole(clientId, handle, name, category, perms :_*)

  def findOrCreateAndSave(clientId: ObjectId, handle: String, name: Box[String], category: String, perms: Permission*): Role =
    findOrCreate(clientId, handle, name, category, perms :_*).save(true)

  def copyDefaultRolesForClient(clientId: ObjectId): List[Role] = {
    for (role <- findDefaultTeamRoles) yield {
      copyClientRole(clientId, role).save(true)
    }
  }

  def findOrCopyClientRoles(clientId: ObjectId): List[Role] = {
    findClientRoles(clientId) match {
      case l: List[Role] if l.nonEmpty => l
      case _ => copyDefaultRolesForClient(clientId)
    }
  }

  def localizeDisplayName(roleHandle: String): Box[String] = {
    val prefix = "userClientConnection.role."
    val localized = tryo(S.?(prefix+roleHandle)).openOr(prefix)
    if (localized.startsWith(prefix)) Empty else Full(localized)
  }

  def displayName(roleHandle: String, roleLocalizedName: Box[String]): String = {
    localizeDisplayName(roleHandle).openOr {
      roleLocalizedName openOr roleHandle
    }
  }

}

/*
 * Simple record for storing roles. Role name is the PK.
 */
class Role private() extends MongoIdRecord[Role] with ClientId[Role] with CreatedUpdated[Role] with SnippetHelpers {
  def meta = Role

  object defaultRole extends BsBooleanField(this) {
    override def defaultValue = false
  }

  object permissionsEditable extends BsBooleanField(this) {
    override def defaultValue = true
  }

  object sortOrder extends BsIntField(this) {
    override def defaultValue = meta.DEFAULT_SORT_ORDER
  }

  /**
   *  A string handle of that role thats not localized
   */
  object roleHandle extends BsStringField(this, 255) {
    override def defaultValue = nextFuncName
  }

  /**
   * The localized name of that role
   */
  object roleLocalizedName extends OptionalBsStringField(this, 255) {
//    override def defaultValue = roleHandle.is
//    override def validations = valMinLen(5, "Bitte geben Sie einen Namen an.") _ :: super.validations
    override def displayName = "Name der Rolle"
    override def asHtml: NodeSeq = Text(owner.displayName)

  private def elem = S.fmapFunc(SFuncHolder(this.setFromAny(_))) {
    println("VALUE: "+valueBox)
    funcName =>
    <input type={formInputType} maxlength={maxLength.toString} class="form-control"
      name={funcName}
      value={valueBox.map(v => if (v.isEmpty) owner.displayName else v) openOr owner.displayName}
      tabindex={tabIndex toString}/>
  }

  override def toForm: Box[NodeSeq] =
    uniqueFieldId match {
      case Full(id) => Full(elem % ("id" -> id))
      case _ => Full(elem)
    }

  }

  object category extends BsStringField(this, 32) {
    override def displayName = "Category"
  }

  // TODO: block editing when not allowed (in handling code)
  object permissions extends PermissionListField(this) {
    override def displayName = "Berechtigungen"
  }

  object memberPermissions      extends ProxyPermsOfRoleField(permissions, Permissions.MemberAll)
  object invoicePermissions     extends ProxyPermsOfRoleField(permissions, Permissions.InvoiceAll)
  object teamPermissions        extends ProxyPermsOfRoleField(permissions, Permissions.TeamAll)
  object notePermissions        extends ProxyPermsOfRoleField(permissions, Permissions.NoteAll)
  object taskPermissions        extends ProxyPermsOfRoleField(permissions, Permissions.TaskAll)
  object eventPermissions       extends ProxyPermsOfRoleField(permissions, Permissions.EventAll)
  object documentPermissions    extends ProxyPermsOfRoleField(permissions, Permissions.DocumentAll)
  object gardenPermissions      extends ProxyPermsOfRoleField(permissions, Permissions.GardenAll)
  object recipientsPermissions  extends ProxyPermsOfRoleField(permissions, Permissions.RecipientsAll)

  override def equals(other: Any): Boolean = other match {
    case r: Role => r.id.get == this.id.get
    case _ => false
  }

  def displayName: String = Role.displayName(roleHandle.get, roleLocalizedName.get)

  def asHtml = {
    val cls = "label" + (roleHandle.get match {
      case RolesDef.R_TEAM_SUPERVISOR => " label-danger"
      case RolesDef.R_TEAM_OWNER      => " label-danger"
      case RolesDef.R_TEAM_MEMBER     => " label-info"
      case _ => " label-default"
    })
    <span class={cls}>{displayName}</span>
  }

}
