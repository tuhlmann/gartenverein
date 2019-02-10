package com.agynamix.garten.model

import net.liftweb.common._
import net.liftweb.util.Helpers._
import com.agynamix.garten.lib.StructuredDbObject
import net.liftweb.json.JsonDSL._
import com.agynamix.garten.model.share.UserId
import net.liftweb.mongodb.record.MongoMetaRecord
import com.agynamix.garten.model.share.MongoIdRecord
import com.agynamix.garten.model.share.CreatedUpdated
import net.liftweb.record.field.StringField
import com.foursquare.rogue.LiftRogue._
import net.liftweb.json.JsonAST.JObject
import net.liftweb.json.JValue
import net.liftweb.json.JValue
import com.mongodb.WriteConcern
import com.agynamix.garten.model.share.FormValidators
import com.agynamix.garten.config.RolesDef
import net.liftweb.util.Helpers
import net.liftweb.record.field.EmailField
import org.bson.types.ObjectId
import com.agynamix.invoice.model.Vat
import com.agynamix.garten.model.share.Attachments
import com.agynamix.garten.snippet.ClientModalDialog
import net.liftweb.http.FileParamHolder
import com.agynamix.garten.api.AllowedTypes
import scala.xml.Text
import com.agynamix.garten.lib.field.BsStringField
import com.agynamix.garten.lib.field.BsEmailField
import com.agynamix.garten.model.share.ProxyDocumentURIField
import net.liftweb.util.BaseField
import com.agynamix.garten.lib.field.BsBooleanField


object Client extends Client with MongoMetaRecord[Client] with StructuredDbObject[Client] with FormValidators[Client] {

  ensureIndex((name.name -> 1), true)

  def keyFields(user: User, args: Any*): Box[()=>JObject] = Full(()=>{
      if (!User.isSuperUser) {
        val clientIds = (Membership where (_.userId eqs user.id.get) select (_.clientId) fetch)
        (id.name -> ("$in" -> clientIds))
      } else {
        JObject(List())
      }
    })

  val DEF_NAME = "default"

  def createInstance(user: User) = {
    val client = Client.createRecord
    client._tmpUser = Full(user)
    client
  }

//  def createDefaultClient(user: User) = {
//    val client = createInstance(user, Empty).name(DEF_NAME).save
//    val teamMember = ensureClientConnectionExists(user, client, UserRoles.TeamOwner.toString())
//    teamMember
//  }

  def findAllByUser(user: User): List[Membership] = (Membership where (_.userId eqs user.id.get) fetch())

  /**
   * FIXME: Create a UserClientConnection only if user asks for it. otherwise
//   * make sure, empty connection works. Create menu entry (Create Client).
   */
  def ensureExistingClientSelected(user: User): Unit = {
    var clientConnections = findAllByUser(user)
    // Create a default client if none exists
//    if (clientConnections.isEmpty) {
//      clientConnections = List(createDefaultClient(user))
//    }
    // Assign the first client in the list if none is assigned
    if (!clientConnections.exists(user.activeMembership.valueBox === _.id.get)) {
      clientConnections.headOption.foreach(c => user.activeMembership(c.id.get).save(true))
    }
  }

  def ensureMembershipExists(user: User, client: Client): Membership =
    ensureMembershipExists(user, client, RolesDef.R_TEAM_MEMBER)


  def ensureMembershipExists(user: User, client: Client, roleHandle: String): Membership =
    ensureMembershipExists(user, client, roleHandle, ConnectionStatus.Connected): Membership

  def ensureMembershipExists(user: User, client: Client, roleHandle: String, conStatus: ConnectionStatus.Value): Membership = {
    Membership.findConnection(user, client).getOrElse{
      logger.info("Creating new TeamMember for user ID "+user.id.get+" and client ID "+client.id.get)
      Membership.createCachedUserInstance(user, client).
                           connectionStatus(conStatus).userRole.findOrSet(client.id.get, roleHandle).save(true)
    }
  }

  def findByName(clientName: String): Box[Client] = Client where (_.name eqs clientName) get()

  def findAllUsers(clientId: ObjectId): List[ObjectId] = {
    Membership where (_.clientId eqs clientId) select(_.userId) fetch
  }

  def findAllMembers(clientId: ObjectId): List[Membership] = {
    Membership where (_.clientId eqs clientId) fetch
  }

  def findAllOwners(clientId: ObjectId): List[User] = {
    (for (ownerRole <- Role.findClientRole(clientId, RolesDef.R_TEAM_OWNER)) yield {
      (Membership where (_.clientId eqs clientId) and (_.userRole eqs ownerRole.id.get) fetch()).flatMap(_.userId.obj)
    }) openOr Nil
  }

  /**
   * Make sure a TeamMember record exists
   * Hook in things that should be checked for new or edited clients
   */
  override def save(client: Client, concern: WriteConcern): Boolean = {
    // println("CLIENT SAVE")
    val re = super.save(client, concern)
    if (re && !User.isSuperUser) {
      for (user <- client._tmpUser) {
        ensureMembershipExists(user, client, RolesDef.R_TEAM_SUPERVISOR)
      }
    }
    User.currentUser.foreach(u => RecipientList.findOrCreateDefaults(u, client))
    Role.findOrCopyClientRoles(client.id.get)
    Vat.findOrCreateStandardVats(client)
    MemberType.findOrCreateStandardTypes(client)
    MemberPosition.findOrCreateStandardPositions(client)
    PropertyTax.findOrCreateStandardTaxes(client)
    User.currentUser.foreach(u => {
      DocumentTemplate.ensureStdDocumentUploadForClient(u, client)
    })
    re
  }

  override def delete_!(client: Client): Boolean = {
    Role.deleteRolesForClient(client.id.get)
    RecipientList.deleteForClient(client.id.get)
    Membership.deleteForClient(client.id.get)
    Vat.deleteForClient(client.id.get)
    // Also delete Member, Garden, ...
    super.delete_!(client)
  }

}

class Client private() extends MongoIdRecord[Client] with CreatedUpdated[Client] with Attachments[Client] {
  def meta = Client

  import Helpers._

  var _tmpUser: Box[User] = Empty
  var _cachedClientConnection: Box[Membership] = Empty

  def uploadDoneCallback = ClientModalDialog.uploadDoneCallback
  def getClientId = this.id.get
  override def maxAttachments = 1
  override def mimeTypeAllowed(fph: FileParamHolder): Option[String] = AllowedTypes.imagesAllowed(fph)

  def clientConnection(user: User): Membership = _cachedClientConnection match {
    case Full(conn) => conn
    case _ =>
      val conn = Client.ensureMembershipExists(user, this)
      _cachedClientConnection = Full(conn)
      conn
  }

  object registerEntry  extends BsStringField(this, 200) {
    override def validations = valMinLen(3, "Bitte geben Sie einen gültigen Registereintrag an.") _ ::
                               meta.uniqueValue(this, "Registereintrag ist nicht eindeutig") _ ::
                               super.validations

    override def displayName = "Registereintrag"
    override def helpAsHtml = Full(Text("Der Registereintrag des Vereins"))
  }

  object name            extends BsStringField(this, 200) {
    override def validations = valMinLen(3, "Bitte geben Sie einen Namen an.") _ ::
                               meta.uniqueValue(this, "Vereinsname ist nicht eindeutig") _ ::
                               super.validations
    override def displayName = "Name des Vereins"
  }

  object street       extends BsStringField(this, 200) {
    override def validations = valMinLen(5, "Bitte geben Sie Straße und Hausnummer an.") _ :: super.validations
    override def displayName = "Straße"
  }

  object city     extends BsStringField(this, 200) {
    override def validations = valMinLen(5, "Bitte geben Sie einen Ort an.") _ :: super.validations
    override def displayName = "Ort"
  }

  object zip             extends BsStringField(this, 5) {
    override def validations = valMinLen(5, "Bitte geben Sie eine gültige Postleitzahl an.") _ ::
    valMaxLen(5, "Bitte geben Sie eine gültige Postleitzahl an.") _ :: super.validations
    override def displayName = "Postleitzahl"
  }

  object districtCourt   extends BsStringField(this, 200) {
    override def validations = valMinLen(3, "Bitte geben Sie einen Namen an.") _ :: super.validations
    override def displayName = "Amtsgericht"
  }

  object phone           extends BsStringField(this, 30) {
    override def validations = valMinLen(3, "Bitte geben Sie eine gültige Telefonnummer an.") _ :: super.validations
    override def displayName = "Telefon"
  }

  object fax             extends BsStringField(this, 30) {
    override def validations = valMinLen(3, "Bitte geben Sie eine gültige Faxnummer an.") _ :: super.validations
    override def displayName = "Fax"
  }

  object web             extends BsStringField(this, 200) {
    override def displayName = "Web Adresse des Vereins"
  }

  object email           extends BsEmailField(this, 254) {
    override def displayName = "Email"
    override def setFilter = trim _ :: toLower _ :: super.setFilter

    override def validations =
      meta.uniqueValue(this, "Es existiert bereits ein Verein mit dieser Emailadresse") _  ::
      valMaxLen(254, "Die Emailadresse darf nicht länger als 254 Zeichen sein.") _ ::
      super.validations
  }

  object logo            extends ProxyDocumentURIField("logo", attachments.objs.headOption)

  object accountOwner extends BsStringField(this, 200) {
    override def displayName = "Kontoinhaber"
  }

  object bankName extends BsStringField(this, 200) {
    override def displayName = "Name der Bank"
  }

  object swift extends BsStringField(this, 200) {
    override def displayName = "Swift-Nr."
  }

  object iban extends BsStringField(this, 200) {
    override def displayName = "Iban-BIC Nr."
  }

  object deductVat extends BsBooleanField(this) {
    override def displayName = "Darf Ust. ausweisen"
    override def defaultValue = false
  }

  override def allRecordFields = logo :: (allFields: List[BaseField])


}
