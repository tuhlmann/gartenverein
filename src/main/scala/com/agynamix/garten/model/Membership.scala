package com.agynamix.garten.model

import net.liftweb.common._
import net.liftweb.util.Helpers._
import net.liftweb.util.FieldContainer
import com.agynamix.garten.model.share._
import com.agynamix.garten.lib.StructuredDbObject
import net.liftweb.json.JsonAST.JObject
import com.agynamix.garten.lib.field.GermanDateField
import scala.xml.Text
import com.agynamix.garten.lib.util.DateHelpers
import net.liftweb.http.SHtml
import scala.xml.NodeSeq
import net.liftweb.http.S
import net.liftweb.http.S.SFuncHolder
import net.liftweb.http.js.JsCmds
import com.agynamix.garten.snippet.{MembershipModalDialog, Gardens, SelectedElements}
import scala.xml.Elem
import java.util.Date
import net.liftweb.mongodb.record.MongoMetaRecord
import net.liftweb.record.field.IntField
import net.liftweb.mongodb.record.field.ObjectIdRefField
import net.liftweb.record.field.StringField
import net.liftweb.record.field.TextareaField
import net.liftweb.record.field.BooleanField
import net.liftweb.mongodb.BsonDSL._
import java.text.SimpleDateFormat
import net.liftweb.json.DefaultFormats
import net.liftweb.record.LifecycleCallbacks
import java.util.Calendar
import com.agynamix.garten.lib.field.DateFormAdapterField
import org.bson.types.ObjectId
import com.foursquare.rogue.LiftRogue._
import net.liftweb.mongodb.record.field.BsonRecordListField
import net.liftweb.mongodb.record.BsonRecord
import net.liftweb.mongodb.record.BsonMetaRecord
import java.util.GregorianCalendar
import net.liftweb.http.js.JsCmds.Alert
import net.liftweb.http.js.JsCmd
import net.liftweb.mongodb.record.field.BsonRecordField
import net.liftweb.record.Field
import org.joda.time.DateTime
import org.joda.time.Years
import net.liftweb.mongodb.record.field.DateField
import net.liftweb.mongodb.record.field.OptionalDateField
import com.agynamix.garten.lib.field.OptionalBirthdayField
import com.agynamix.garten.lib.field.GenderField
import com.mongodb.WriteConcern
import net.liftweb.record.field.OptionalEmailField
import com.agynamix.garten.lib.Permission
import com.agynamix.garten.config.RolesDef
import scala.xml.Node
import net.liftweb.record.field.EmailField
import net.liftweb.util.BaseField
import net.liftweb.util.FieldError
import com.agynamix.garten.lib.field.BsStringField
import com.agynamix.garten.lib.field.BsObjectIdRefField
import com.agynamix.garten.lib.field.OptionalBsEmailField
import com.agynamix.garten.lib.field.BsEnumNameField
import com.agynamix.garten.lib.field.BsBooleanField
import com.agynamix.garten.lib.field.BsTextareaField
import com.agynamix.garten.lib.field.BsEmailField
import com.agynamix.garten.lib.util.LocalizedEnum
import com.agynamix.garten.lib.field.BsObjectIdRefField
import com.agynamix.garten.lib.field.BsIntField
import com.agynamix.garten.lib.MongoDataSourceTrait
import org.joda.money.Money
import org.joda.money.CurrencyUnit
import com.agynamix.document.MarkInvoicedData
import com.agynamix.garten.lib.field.ItemInvoicedMarkerList
import com.agynamix.invoice.model.GeneratedInvoice
import com.agynamix.invoice.model.InvoiceContainer
import net.liftweb.json.JsonAST.JObject
import com.agynamix.document.MarkInvoicedData
import scala.Some
import net.liftweb.http.js.JsCmds.Alert
import net.liftweb.common.Full
import java.util.regex.Pattern


object ConnectionStatus extends Enumeration with LocalizedEnum {
  type ConnectionStatusType = Value

  val locKeyBase = "ConnectionStatus.status"

  val Connected = Value("Connected")
  val Invited = Value("Invited")

  def asHtml(in: ConnectionStatusType): Node = {
    val cls = "label" + (in match {
      case Connected => " label-info"
      case _ => " label-default"
    })
    <span class={cls}>{localized(in)}</span>
  }

}

class Address extends BsonRecord[Address] {
  def meta = Address

  object street       extends BsStringField(this, 200) {
    override def validations = valMinLen(5, "Bitte geben Sie Straße und Hausnummer an.") _ :: super.validations
    override def displayName = "Straße"
  }

  object zip          extends BsStringField(this, 5) {
    override def validations = valMinLen(5, "Bitte geben Sie eine gültige Postleitzahl an.") _ ::
    valMaxLen(5, "Bitte geben Sie eine gültige Postleitzahl an.") _ :: super.validations
    override def displayName = "Postleitzahl"
  }

  object city         extends BsStringField(this, 200) {
    override def validations = valMinLen(5, "Bitte geben Sie einen Ort an.") _ :: super.validations
    override def displayName = "Ort"
  }

  object validSince extends DateField(this) with DateFormAdapterField with GermanDateField {
    override def displayName = "Gültig Seit"
    override def defaultValue = new Date()
  }

  def stringRep: String = {
    s"${street.get}, ${zip.get} ${city.get}"
  }

}

object Address extends Address with BsonMetaRecord[Address] with Loggable {

  def createInstance() = Address.createRecord

  def newAddress(street: String, zip: String, city: String): Address =
    Address.createRecord.street(street).zip(zip).city(city)

  def current(adr: List[Address]): Box[Address] = adr.headOption

  def currentAddressForm(adr: List[Address]) = {
    val id = nextFuncName
    <span>
      <input id={id} type="text" readonly="readonly" class="value input-xlarge"
             value={ adr.headOption.map(_.stringRep).getOrElse("")} />
      {SHtml.ajaxButton("...",
          ()=>Alert("Hi"), "class"->"btn value")}
    </span>
  }

  def headOrCreate(addresses: BsonRecordListField[_, Address]): Address = {
    addresses.get.headOr {
      logger.info("XX Create new Address Record")
      val adr = Address.createRecord
      addresses.atomicUpdate(lst => adr :: lst)
      adr
    }
  }

//  def chooseItem(onSel: Address=>JsCmd): JsCmd = {
//    new Adresses().chooseItem(onSel)
//  }


}

object Membership extends Membership with LogbookMetaRecord[Membership] with StructuredDbObject[Membership] with FormValidators[Membership] {

  ensureIndex((clientId.name -> 1))

  def keyFields(user: User, args: Any*): Box[()=>JObject] =
    user.activeMembership.clientId.map(id => ()=> (clientId.name -> id) )

  def createInstance(client: Client): Membership = {
    val membership = Membership.createRecord.clientId(client.id.get).addresses(List(Address.createInstance))
    val newUser = User.createInstance.isNewRecord(true)
    membership._cachedUser = Full(newUser)
    membership.userId(newUser.id.get)
    newUser.activeMembership(membership.id.get)
    membership
  }

  def createCachedUserInstance(membershipUser: User, client: Client) = {
    val membership = Membership.createRecord.clientId(client.id.get).addresses(List(Address.createInstance))
    membership._cachedUser = Full(membershipUser)
    membership.userId(membershipUser.id.get)
    membershipUser.activeMembership(membership.id.get)
    membership
  }

  def membershipValidates(user: User): Boolean = {
    val re = (for (membership <- user.activeMembership.obj) yield {
      val re = membership.validate
      //println("Found Membership, validating: "+re)
      re.isEmpty
    }) openOr true

    //println("Membership validates: "+re)
    re
  }

  def findGardenOwners(gardenId: ObjectId): List[Membership] = (Membership where (_.gardenRef eqs gardenId) fetch)

  def findGardenOwnersContractorsFirst(gardenId: ObjectId): List[Membership] = {
    val all = findGardenOwners(gardenId).sortWith{case (m1, m2) =>
      (for { mt1 <- m1.memberType.obj
            mt2 <- m2.memberType.obj } yield {
        mt1.isContractor.get
      }) openOr false
    }
    all
  }

  def findFirstTwoGardenOwnerNames(gardenId: ObjectId, delim: String): String = {
    val all = findGardenOwnersContractorsFirst(gardenId)
    val re = all.take(2).map(_.displayName).mkString(delim)
    if (all.size > 2) re + ", ..." else re
  }

  //def findByEmail(clientId: ObjectId, email: String) = Membership where (_.clientId eqs clientId) and (_.email eqs email) get()

  def findConnection(user: User, client: Client): Box[Membership] = findConnection(user.id.get, client.id.get)
  def findConnection(userId: ObjectId, clientId: ObjectId): Box[Membership] =
    (Membership where (_.userId eqs userId) and (_.clientId eqs clientId) get())

  def countTeamMembersWithRole(clientId: ObjectId, roleHandle: String): Long = {
    (for (role <- Role.findClientRole(clientId, roleHandle)) yield {
      (Membership where (_.clientId eqs clientId) and (_.userRole eqs role.id.get) count)
    }) openOr 0
  }

  def findAllMembers(clientId: ObjectId): List[Membership] = (Membership where (_.clientId eqs clientId) fetch)

  def upcomingBirthdays(clientId: ObjectId, forwardDays: Int, maxItems: Int): List[Membership] = {
    val dateNow = new DateTime()
    val dateForward = new DateTime().plusDays(forwardDays)
    //println("Find members for "+clientId)
    val re = findAllMembers(clientId) filter (ms => {
      (for (bd <- ms.birthday.valueBox) yield {
        val d = new DateTime(bd).withYear(dateNow.getYear())
        val re = d.isAfter(dateNow) && d.isBefore(dateForward)
        //println(s"Filter: ${ms.displayName} with bd: ${bd} is ${re}")
        re
      }) openOr false
    }) sortWith {case (ms1, ms2) => {
      (ms1.birthday.get, ms2.birthday.get) match {
        case (Some(bd1), Some(bd2)) =>
          val d1 = new DateTime(bd1).withYear(dateNow.getYear())
          val d2 = new DateTime(bd2).withYear(dateNow.getYear())
          d1.getMillis() < d2.getMillis()
        case _ => false
      }
    }}
    re.take(maxItems)
  }

  def yearlyPayment(member: Membership): Money = {
    member.memberType.obj.map(_.yearlyPayment.get).openOr(MemberType.yearlyPayment.defaultValue)
  }

  def markValuesInvoiced(invoice: InvoiceContainer, generatedInvoice: GeneratedInvoice, recipient: Membership): Unit = {
    recipient.yearlyInvoices.addMarker(invoice.id.get, generatedInvoice.invoiceNo.get,
        invoice.invoiceYear.get, invoice.invoiceDate.get);
  }

  def deleteForClient(clientId: ObjectId) =
    findAllMembers(clientId) foreach (_.delete_!)


  /**
   * Make sure all the depending records get saved
   * Hook in things that should be checked for new or edited clients
   */
  override def save(member: Membership, concern: WriteConcern): Boolean = {
    //println("Member SAVE")
    member._cachedUser.foreach(_.save(true))
    val re = super.save(member, concern)
    re
  }

  override def validate(inst: Membership): List[FieldError] = {
    foreachCallback(inst, _.beforeValidation)
    try{
      //fieldList.flatMap(_.field(inst).validate)
      inst.allRecordFields.flatMap(_.validate)
    } finally {
      foreachCallback(inst, _.afterValidation)
    }
  }

}

class Membership private() extends MongoIdRecord[Membership] with ClientId[Membership]
                           with CreatedUpdated[Membership] with Selectable[Membership]
                           with Attachments[Membership]
                           with MongoDataSourceTrait[Membership] {
  def meta = Membership

  def getClientId = this.clientId.get
  def uploadDoneCallback = MembershipModalDialog.uploadDoneCallback
  override def isDocumentHidden = false

  def datasourceModule = "member"

  private var _cachedUser: Box[User] = Empty

  def displayName = if (firstname.get.nonEmpty || lastname.get.nonEmpty) s"${firstname.get} ${lastname.get}" else ""

  def lastAndFirstName = s"${lastname.get}, ${firstname.get}"

  def clientName: String = clientId.obj.map(_.name.get) openOr ""

  def address: Address = Address.current(addresses.get) openOr Address.createRecord

  object member_no extends BsIntField(this) {
    override def validations = meta.uniqueValue(this, "Mitgliedsnummer ist nicht eindeutig") _ :: super.validations
    override def displayName = "Mitgliedsnr."
    override def defaultValue = meta.nextIntValue(this, 1)
  }

  object userId extends BsObjectIdRefField(this, User) {
    def setCache(u: User) = owner._cachedUser = Full(u)

    override def obj = {
      owner._cachedUser or {
        owner._cachedUser = super.obj
        owner._cachedUser
      }
    }
  }

  object userRole extends BsObjectIdRefField(this, Role) {
    override def displayName = "Rolle"
    def permissions: List[Permission] = obj.map(_.permissions.get).openOr(Nil)
    def roleHandle: String = obj.map(_.roleHandle.get).openOr("")
    def roleLocalizedName: String = obj.flatMap(_.roleLocalizedName.get).openOr("")
    override def asHtml = obj.map(_.asHtml) openOr Text("")

    def roleDisplayName = obj.map(_.displayName) openOr ""

    override def options: List[(Box[ObjectId], String)] = {
      {
        val r = Role.findClientRoles(clientId.get)
        val r2 = if (roleHandle != RolesDef.R_TEAM_SUPERVISOR) r.filterNot(_.roleHandle.get == RolesDef.R_TEAM_SUPERVISOR) else r
        r2.sortWith((a, b) => a.sortOrder.get < b.sortOrder.get)
      } map (r => (Full(r.id.get), r.displayName))
    }

    def defaultRoleId =
      obj.map(_.id.get) or Role.findClientRoles(clientId.get).find(_.roleHandle.get == RolesDef.R_TEAM_MEMBER).map(_.id.get)

    override def toForm: Box[NodeSeq] = {
      val opt = buildDisplayList.flatMap(r => r._1.map((_, r._2)))
      Full(SHtml.selectObj[ObjectId](opt, defaultRoleId, v => this.set(v), "class" -> "form-control"))
    }

    def findOrSet(clientId: ObjectId, roleHandle: String): Membership = {
      val role = Role.findOrCreateAndSave(clientId, roleHandle, Empty, RolesDef.CAT_TEAM, RolesDef.P_TEAM_MEMBER.toSeq :_*)
      this.set(role.id.get)
      owner
    }

  }

  object gardenRef extends OptionalObjectIdRefListField(this, Garden) {
//    override def validations = Garden.valueExists(this, Garden.id, "Ein Garten mit dieser Nummer existiert nicht") _ ::
//                                super.validations
    override def displayName = "Gartennummer"

    override def toForm =
      uniqueFieldId match {
        case Full(id) => Full(elem % ("id" -> id))
        case _ => Full(elem)
      }

    override def asHtml = objs match {
      case l: List[Garden] if l.nonEmpty => Text(l.map(_.garden_no.get).mkString(", "))
      case _ => Text("Keine")
    }

    private def addGardenIds(selected: List[ObjectId], unselected: List[ObjectId]): List[ObjectId] = {
      val cleaned = this.get.filterNot(unselected.contains)
      (cleaned ::: selected).distinct
    }

    private def valueToShow = objs.map(_.garden_no.get.toString).mkString(", ")

    def elem = {
        val id = nextFuncName
        <span>
          <input id={id} type={formInputType} readonly="readonly" class="value input-medium form-control"
                 style="display:inline-table;" value={ valueToShow } />
          {SHtml.ajaxButton(<i class="icon-plus"></i>, ()=>new Gardens().chooseItem(true, this.valueBox.openOr(Nil), {
            case SelectedElements(selected, unselected) =>
             this.set(addGardenIds(selected, unselected)); JsCmds.SetValById(id, valueToShow)
            case _ => JsCmds.Noop
            }, "gardens-item-chooser"), "class"->"btn btn-default value")}
          {SHtml.ajaxButton(<i class="icon-trash"></i>, ()=>{this.setBox(Empty); JsCmds.SetValById(id, "")}, "class"->"btn btn-danger value")}
        </span>
      }

  }

  object gender extends GenderField(this)

  object lastname     extends ProxyStringField(User.lastname, userId.obj.map(_.lastname)) {
    //override def validations = valMinLen(2, "Bitte geben Sie einen Nachnamen an.") _ :: super.validations
    override def displayName = "Nachname"
  }

  object firstname     extends ProxyStringField(User.firstname, userId.obj.map(_.firstname)) {
    //override def validations = valMinLen(2, "Bitte geben Sie einen Vornamen an.") _ :: super.validations
    override def displayName = "Vorname"
  }

  object userEmail     extends ProxyUserEmailField(userId) {
    override def displayName = "Email"
  }

  // used when creating new members
  object tmpEmail     extends OptionalBsEmailField(this, 200) {
    override def uniqueFieldId: Box[String] = User.email.uniqueFieldId
    override def ignoreField_? = true
    override def displayName = "Email"
    override def defaultValueBox = userEmail.get
  }

  object birthday  extends OptionalBirthdayField(this)

  object addresses extends BsonRecordListField(this, Address) {
    override def displayName = "Aktuelle Adresse"

    override def toForm =
      uniqueFieldId match {
        case Full(id) => Full(Address.currentAddressForm(get) % ("id" -> id))
        case _ => Full(Address.currentAddressForm(get))
      }
  }

  object street extends ProxyBsonField[Address](Address.headOrCreate(addresses).street) {
    override def displayName = "Straße"
  }

  object zip extends ProxyBsonField[Address](Address.headOrCreate(addresses).zip) {
    override def displayName = "Postleitzahl"
  }

  object city extends ProxyBsonField[Address](Address.headOrCreate(addresses).city) {
    override def displayName = "Ort"
  }

//  override def allRecordFields = street :: zip :: city :: proxiedName :: firstname :: lastname :: userEmail :: (allFields: List[BaseField])
  override def allRecordFields = street :: zip :: city :: firstname :: lastname :: userEmail :: (allFields: List[BaseField])

  object phone     extends BsStringField(this, 200) {
    // optional
    override def displayName = "Telefon"
  }

  object mobile     extends BsStringField(this, 200) {
    // optional
    override def displayName = "Mobiltelefon"
  }

  object memberJoin extends DateField(this) with DateFormAdapterField with GermanDateField {
    override def displayName = "Mitglied Start"

    def memberSince: String = {
      (for (v <- valueBox) yield {
        val join = new DateTime(v)
        val now = new DateTime()
        val since = Years.yearsBetween(join, now).getYears();
        s"seit ${since.toString()} ${if (since == 1) "Jahr" else "Jahren"} Mitglied"
      }) openOr ""
    }
  }

  object memberLeave  extends OptionalDateField(this) with DateFormAdapterField with GermanDateField {
    override def displayName = "Mitglied Ende"
  }

  object memberType extends BsObjectIdRefField(this, MemberType) {
    override def displayName = "Art der Mitgliedschaft"
    override def asHtml = Text(obj.map(_.displayName) openOr "")

    override def options: List[(Box[ObjectId], String)] = {
      MemberType.findMemberTypes(clientId.get).
                 sortWith((a, b) => a.sortOrder.get < b.sortOrder.get).
                 map (r => (Full(r.id.get), r.displayName))
    }

    override def toForm: Box[NodeSeq] = {
      val opt = buildDisplayList.flatMap(r => r._1.map((_, r._2)))
      Full(SHtml.selectObj[ObjectId](opt, obj.map(_.id.get), v => this.set(v), "class" -> "form-control"))
    }

    def isContractor = obj.map(_.isContractor.get) openOr false

  }

  object memberPosition extends BsObjectIdRefField(this, MemberPosition) {
    override def displayName = "Position im Verein"
    override def asHtml = Text(obj.map(_.displayName) openOr "")

    override def options: List[(Box[ObjectId], String)] = {
      MemberPosition.findMemberPositions(clientId.get).
                 sortWith((a, b) => a.sortOrder.get < b.sortOrder.get).
                 map (r => (Full(r.id.get), r.displayName))
    }

    override def toForm: Box[NodeSeq] = {
      val opt = buildDisplayList.flatMap(r => r._1.map((_, r._2)))
      Full(SHtml.selectObj[ObjectId](opt, obj.map(_.id.get), v => this.set(v), "class" -> "form-control"))
    }
  }

  object isActiveMember  extends BsBooleanField(this) {
    override def defaultValue = true
    override def displayName = "Aktives Mitglied"
  }

  object note  extends BsTextareaField(this, 1000) {
    override def displayName = "Bemerkung"
  }

  object invitationMsg extends BsTextareaField(this, 1024) {
    override def textareaRows = 4
  }

  object connectionStatus extends BsEnumNameField(this, ConnectionStatus) {
    override def displayName = "Status"
    override def defaultValue = ConnectionStatus.Invited
    override def asHtml = ConnectionStatus.asHtml(get)
    def isConnected = get == ConnectionStatus.Connected
    def isInvited = get == ConnectionStatus.Invited

    override def buildDisplayList: List[(Box[ConnectionStatus.Value], String)] = {
      (Full(ConnectionStatus.Invited), "Eingeladen") :: (Full(ConnectionStatus.Connected), "Verbunden") :: Nil
    }

//      def buildDisplayList: List[(Box[EnumType#Value], String)] = {
//    val options = enum.values.toList.map(a => (Full(a), a.toString))
//    if (optional_?) (Empty, emptyOptionLabel)::options else options
//  }


  }

  /**
   * One record per appearance on an invoice is created
   */
  object yearlyInvoices extends ItemInvoicedMarkerList(this)

  override def clientGetRawProperty = {
    case "yearlyPayment" => meta.yearlyPayment(this)
    case "displayName" => displayName
  }

  override def markValueInvoiced(key: String, markInvoicedData: MarkInvoicedData): Unit = key match {
    //case "yearlyPayment"      =>
      //yearlyPaymentInvoiced.addMarker(markInvoicedData.invoiceId, markInvoicedData.generatedInvoiceNo, markInvoicedData.invoiceDate)

    case _ =>
  }


}