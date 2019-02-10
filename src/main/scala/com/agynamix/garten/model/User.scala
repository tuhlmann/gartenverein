package com.agynamix.garten.model

import org.joda.time.DateTime
import net.liftweb._
import net.liftweb.common._
import net.liftweb.util.Helpers._
import net.liftweb.util.FieldContainer
import com.agynamix.garten.lib.plan.Free
import net.liftweb.util.Mailer.From
import net.liftweb.util.Mailer.PlainMailBodyType
import net.liftweb.util.Mailer.Subject
import net.liftweb.util.Mailer.To
import net.liftweb.util.Mailer.sendMail
import net.liftweb.http.LiftResponse
import net.liftweb.http.RedirectWithState
import net.liftweb.http.RedirectState
import net.liftweb.http.Req
import net.liftweb.http.SessionVar
import net.liftweb.http.S
import net.liftweb.http.RedirectResponse
import com.agynamix.garten.config.GardenConfig
import net.liftweb.record.field.LocaleField
import net.liftweb.record.field.TimeZoneField
import net.liftweb.record.field.StringField
import net.liftweb.mongodb.record.field.ObjectIdRefField
import net.liftweb.mongodb.record.field.ObjectIdPk
import net.liftweb.record.field.TextareaField
import org.bson.types.ObjectId
import com.agynamix.garten.model.share.CreatedUpdated
import com.agynamix.garten.lib.StructuredDbObject
import net.liftweb.json.JsonAST._
import com.agynamix.garten.model.share.MongoIdRecord
import scala.xml.Text
import scala.xml.NodeSeq
import net.liftweb.http.SHtml
import net.liftweb.http.SHtml._
import com.agynamix.garten.snippet.Clients
import net.liftweb.http.js.JsCmds
import com.agynamix.garten.config.RolesDef
import com.foursquare.rogue.LiftRogue._
import net.liftweb.util.Props
import com.agynamix.garten.config.Permissions
import com.agynamix.garten.lib.MailSender
import com.agynamix.garten.lib.ProtoAuthUser
import com.agynamix.garten.lib.ProtoAuthUserMeta
import com.agynamix.garten.lib.Permission
import net.liftweb.record.field.BooleanField
import com.agynamix.garten.model.share.MembershipProxyField
import org.joda.time.DateTimeZone
import com.agynamix.garten.lib.field.{OptionalBsStringField, BsBooleanField, BsTimeZoneField}
import java.util.{Locale, TimeZone}


class User private () extends ProtoAuthUser[User] with MongoIdRecord[User] with CreatedUpdated[User] {
  def meta = User

  override def userIdAsString: String = id.toString

  object locale extends LocaleField(this) {
    override def displayName = "Land"
    override def defaultValue = "de_DE"
  }
  object timezone extends BsTimeZoneField(this) {
    override def displayName = "Zeitzone"
    override def defaultValue = "Europe/Berlin"
  }

  object location extends StringField(this, 64) {
    override def displayName = "Ort"

    override def validations =
      valMaxLen(64, "Location must be 64 characters or less") _ ::
      super.validations
  }

//  object bio extends TextareaField(this, 160) {
//    override def displayName = "Bio"
//
//    override def validations =
//      valMaxLen(160, "Bio must be 160 characters or less") _ ::
//      super.validations
//  }

  /**
   * Associate a payment plan with the user. Default is the free plan
   */
  object paymentPlan extends StringField(this, 100) {
    override val defaultValue = Free.name
  }

  private var _cachedMembership: Box[Membership] = Empty

  object activeMembership extends ObjectIdRefField(this, Membership) {
    override def obj = {
      owner._cachedMembership or {
        owner._cachedMembership = super.obj
        owner._cachedMembership
      }
    }
    def setWithCache(ms: Membership) = {
      _cachedMembership = Full(ms)
      set(ms.id.get)
    }

    override def displayName = "Verein"
    override def optional_? = true
    def activeClient: Box[Client] = obj.flatMap(_.clientId.obj)
    def clientId = activeClient.map(_.id.get)

//    override def validations = UserClientConnection.valueExists(this, UserClientConnection.id, "Ein Verein mit dieser Nummer existiert nicht") _ ::
//                                super.validations

    override def toForm =
      uniqueFieldId match {
        case Full(id) => Full(elem % ("id" -> id))
        case _ => Full(elem)
      }

    def elem = {
        val id = nextFuncName
        <span>
          <input id={id} type={formInputType} readonly="readonly" class="value input-xlarge"
                 value={ obj.flatMap(_.clientId.obj.map(_.name.get.toString)).openOr("")} />
          {SHtml.ajaxButton("...", ()=>Clients.chooseItem(false, {
            case c: Client =>
              val ucc = Client.ensureMembershipExists(owner, c, RolesDef.R_TEAM_OWNER, ConnectionStatus.Invited)
              this.set(ucc.id.get); JsCmds.SetValById(id, c.name.get.toString)
            case _ => JsCmds.Noop
          }), "class"->"btn value")}
        </span>
      }

    override def asHtml: NodeSeq = Text(obj.map(_.clientName).openOr(""))

  }

  object roleInActiveClientConnection extends MembershipProxyField(activeMembership.obj)

  object twoFactorAuthenticationKey extends OptionalBsStringField(this, 100) {
    override def displayName = "Zwei-Faktor-Authentifizierung"
  }

  def useTwoFactorAuthentication(): Boolean = {
    twoFactorAuthenticationKey.valueBox.isDefined
  }

  object isNewRecord extends BooleanField(this) {
    override def ignoreField_? = true
    override def defaultValue = false
  }

  /*
   * FieldContainers for various LiftScreeens.
   */
  def accountScreenFields = new FieldContainer {
    def allFields = List(firstname, lastname, email)
  }

  def profileScreenFields = new FieldContainer {
    def allFields = List(firstname, lastname, location)
  }

  def registerScreenFields = new FieldContainer {
    def allFields = List(firstname, lastname, email)
  }

  def whenCreated: DateTime = new DateTime(id.get.getTime)

  def switchActiveMembership(membership: Membership): User = {
    this.activeMembership(membership.id.get).save(true)
  }

  def switchActiveClient(client: Client): User = {
    for (membership <- Membership.findConnection(this, client)) {
      this.activeMembership(membership.id.get).save(true)
    }
    this
  }

  def activeClientRole: Box[Role]               = activeMembership.obj.flatMap(_.userRole.obj)
  def activeClientPermissions: List[Permission] = activeMembership.obj.map(_.userRole.permissions).openOr(Nil)

  def authRoles: Set[Role] = {
    val s = roles.objs.toSet
    //println("USER Attached Roles: "+s.map(_.roleHandle.is).mkString(", "))
    (for (r <- activeClientRole) yield {
      //println("Client Attached Roles: "+r.roleHandle.is)
      val re = s + r
      //println("ALL Attached Roles: "+re.map(_.roleHandle.is).mkString(", "))
      re
    }) openOr s
  }

  def authPermissions: Set[Permission] = (permissions.get ::: roles.permissions ::: activeClientPermissions).toSet

}

object User extends User with ProtoAuthUserMeta[User] with StructuredDbObject[User] with Loggable {

  import mongodb.BsonDSL._

  ensureIndex((email.name -> 1), true)
//  ensureIndex((username.name -> 1), true)

  def createInstance = User.createRecord

  def keyFields(user: User, args: Any*): Box[()=>JObject] = Full(()=>{
      args.headOption match {
        case Some(Full(c: Client)) =>
          val userIds = (Membership where (_.clientId eqs c.id.get) select (_.userId) fetch)
          (id.name -> ("$in" -> userIds))
        case _ => JObject(List())
      }
    })

  def isSuperUser: Boolean = User.hasPermission(Permissions.SuperUser)

  def getDateTimeZone = User.currentUser.map(_.timezone.isAsJodaTimeZone) openOr DateTimeZone.getDefault()
  def getTimeZone = User.currentUser.map(_.timezone.isAsTimeZone) openOr TimeZone.getDefault()
  def getLocale   = User.currentUser.map(_.locale.isAsLocale) openOr Locale.getDefault

  def findByEmail(in: String): Box[User] = find(email.name, in)

  def findByStringId(id: String): Box[User] =
    if (ObjectId.isValid(id)) find(new ObjectId(id))
    else Empty

  override def onLogIn: List[User => Unit] = List(
      user => User.loginCredentials.remove(),
      user => Client.ensureExistingClientSelected(user),
      user => Logbook.logUserLoggedIn(user)
  )

  override def onLogOut: List[Box[User] => Unit] = List(
    boxedUser => boxedUser.foreach(u => ExtSession.deleteExtCookie()),
    boxedUser => boxedUser.foreach(u => Logbook.logUserLoggedOut(u))
  )

  /*
   * MongoAuth vars
   */
  private lazy val siteName                = GardenConfig.siteName.vend
  private lazy val sysName                 = GardenConfig.systemName.vend
  private lazy val indexUrl                = GardenConfig.indexUrl.vend
  private lazy val registerUrl             = GardenConfig.registerUrl.vend
  private lazy val setPasswordUrl          = GardenConfig.setPasswordUrl.vend
  private lazy val loginTokenAfterUrl      = GardenConfig.loginTokenAfterUrl.vend
  private lazy val invitationTokenAfterUrl = GardenConfig.invitationTokenAfterUrl.vend

  /*
   * LoginToken
   */
  override def handleLoginToken: Box[LiftResponse] = {
    val resp = S.param("token").flatMap(LoginToken.findByStringId) match {
      case Full(at) if (at.expires.isExpired) => {
        at.delete_!
        RedirectWithState(indexUrl, RedirectState(() => { S.error("Login token has expired") }))
      }
      case Full(at) => find(at.userId.get).map(user => {
        if (user.validate.length == 0) {
          user.verified(true)
          user.save(true)
          //logUserIn(user)
          //at.delete_!
          //RedirectResponse(loginTokenAfterUrl)
          regUser(user)
          regToken(Full(at))
          regUserFields(CRegUserFields(false, false, false))
          RedirectWithState(setPasswordUrl, RedirectState(() => { S.notice("Bitte vergeben Sie ein neues sicheres Passwort.") }))
        }
        else {
          at.delete_!
          regUser(user)
          RedirectWithState(registerUrl, RedirectState(() => { S.notice("Bitte registrieren Sie sich.") }))
        }
      }).openOr(RedirectWithState(indexUrl, RedirectState(() => { S.error("Nutzer nicht gefunden") })))
      case _ => RedirectWithState(indexUrl, RedirectState(() => { S.warning("Logintoken nicht gefunden") }))
    }

    Full(resp)
  }

  /*
   * InvitationToken
   */
  override def handleInvitationToken: Box[LiftResponse] = {
    val resp = S.param("token").flatMap(LoginToken.findByStringId) match {
      case Full(at) if (at.expires.isExpired) => {
        at.delete_!
        RedirectWithState(indexUrl, RedirectState(() => { S.error(S ? "invitation.token.expired") }))
      }
      case Full(at) => Membership.find(at.invitationId.get).map(membership => {

        find(membership.userId.get).map(user => {
          if (user.validate.length == 0) {
            user.verified(true)
            user.save(true)
            //logUserIn(user)
            //at.delete_!
            println("Set Membership Connected")
            membership.connectionStatus(ConnectionStatus.Connected).save(true)
            MailSender.sendUserAcceptedInvitation(user, membership)
            regUser(user)
            regToken(Full(at))
            regUserFields(CRegUserFields(true, true, true))
            RedirectWithState(setPasswordUrl, RedirectState(() => { S.notice("Bitte vervollständigen Sie Ihre Anmeldung.") }))
          } else {
            at.delete_!
            regUser(user)
            RedirectWithState(registerUrl, RedirectState(() => { S.notice("Bitte registrieren Sie sich.") }))
          }
        }).openOr(RedirectWithState(indexUrl, RedirectState(() => { S.error("Nutzer nicht gefunden") })))

      }).openOr(RedirectWithState(indexUrl, RedirectState(() => { S.error("Einladung nicht gefunden") })))
      case _ => RedirectWithState(indexUrl, RedirectState(() => { S.warning("Einladungsschlüssel nicht gefunden") }))
    }

    Full(resp)
  }

  /*
   * ExtSession
   */
  def createExtSession(uid: String) = ExtSession.createExtSession(uid)

  /*
  * Test for active ExtSession.
  */
  def testForExtSession: Box[Req] => Unit = {
    ignoredReq => {
      if (currentUserId.isEmpty) {
        ExtSession.handleExtSession match {
          case Full(es) => find(es.userId.get).foreach { user => logUserIn(user, false) }
          case Failure(msg, _, _) => logger.warn("Error logging user in with ExtSession: %s".format(msg))
          case Empty =>
        }
      }
    }
  }

  // used during login process
  object loginCredentials extends SessionVar[LoginCredentials](LoginCredentials(""))
  object regUser  extends SessionVar[User](createRecord.email(loginCredentials.is.email))
  object regToken extends SessionVar[Box[LoginToken]](Empty)
  object regUserFields extends SessionVar[CRegUserFields](CRegUserFields())

  case class CRegUserFields(firstname: Boolean = true, lastname: Boolean= true, email: Boolean = true)

}

case class LoginCredentials(val email: String, val isRememberMe: Boolean = false)

object SystemUser extends Loggable {
  private val email = GardenConfig.mailerFromAddress.vend //Props.get("system.email.address", "gartenhans@agynamix.de")

  lazy val user: User = User.find(User.email.name, email) openOr {

    // find or create superuser role
    val suRole = Role.findOrCreateAndSaveSU()

    val u = User.createRecord
      .firstname("Admin")
      .lastname("unser-gartenverein.de")
      .email(email)
      .locale("de_DE")
      .timezone("Europe/Berlin")
      .verified(true)
      .password(Props.get("system.user.password", nextFuncName))
      .roles(List(suRole.id.get))
    u.password.hashIt
    u.save(true)
  }
}
