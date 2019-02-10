package com.agynamix.garten.model

import net.liftweb.common._
import net.liftweb.util.Helpers._
import net.liftweb.util.FieldContainer
import com.agynamix.garten.model.share.UserId
import com.agynamix.garten.model.share.ClientId
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
import com.agynamix.garten.snippet.Gardens
import scala.xml.Elem
import java.util.Date
import net.liftweb.mongodb.record.MongoMetaRecord
import com.agynamix.garten.model.share.CreatedUpdated
import com.agynamix.garten.model.share.MongoIdRecord
import net.liftweb.record.field.IntField
import net.liftweb.mongodb.record.field.ObjectIdRefField
import net.liftweb.record.field.StringField
import net.liftweb.record.field.TextareaField
import net.liftweb.record.field.BooleanField
import net.liftweb.mongodb.BsonDSL._
import com.agynamix.garten.model.share.FormValidators
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
import com.agynamix.garten.model.share.ProxyBsonField
import net.liftweb.mongodb.record.field.BsonRecordField
import net.liftweb.record.Field
import org.joda.time.DateTime
import org.joda.time.Years
import net.liftweb.http.FileParamHolder
import net.liftweb.http.SHtml.ElemAttr
import net.liftweb.http.S.BinFuncHolder
import net.liftweb.http.js.JsCmds._
import net.liftweb.record.TypedField
import net.liftweb.record.BaseField
import com.agynamix.garten.config.GardenConfig
import com.agynamix.garten.service.DocumentInfo
import net.liftweb.record.field.LongField
import net.liftweb.mongodb.record.field.UUIDField
import java.io.File
import java.util.UUID
import com.agynamix.garten.api.FileUploadInProgress
import net.liftweb.http.js.JE.AnonFunc
import com.agynamix.garten.snippet.StructuredLiftScreen
import com.agynamix.garten.snippet.StructuredLiftScreen
import com.agynamix.garten.snippet.NoteModalDialog
import net.liftweb.mongodb.record.field.ObjectIdRefListField
import com.agynamix.garten.snippet.Memberships
import net.liftweb.mongodb.record.field.MongoCaseClassListField
import com.agynamix.garten.snippet.SelectedElements
import com.agynamix.garten.lib.field.BsBooleanField
import com.agynamix.garten.lib.field.BsEnumNameField
import com.agynamix.garten.lib.field.BsStringField
import com.agynamix.garten.lib.field.BsIntField
import com.agynamix.garten.lib.util.LocalizedEnum
import com.agynamix.garten.model.share.Recipients

case class RecipientData(displayName: String, email: String, userId: ObjectId)

object RecipientListType extends Enumeration with LocalizedEnum {
  type RecipientListType = Value

  def locKeyBase = "recipients.type"

  val Private    = Value("private")
  val All        = Value("all")
  val Person     = Value("person")
  val Manual     = Value("manual")

  def sortOrder(t: RecipientListType.Value): Int = t match {
    case Private => 1
    case All     => 2
    case Person  => 3
    case Manual  => 99
  }

  def listTypeName(t: RecipientListType.Value): String = t match {
    case Manual => "recipients.type.name.manual"
    case _      => "recipients.type.name.smart"
  }

}

object RecipientList extends RecipientList with MongoMetaRecord[RecipientList] with StructuredDbObject[RecipientList] with FormValidators[RecipientList] {

  ensureIndex((clientId.name -> 1), false)

  def keyFields(user: User, args: Any*): Box[() => JObject] =
    user.activeMembership.clientId.map(id => () => (clientId.name -> id))

  def createInstance(user: User, client: Client) = {
    RecipientList.createRecord.userId(user.id.get).clientId(client.id.get).listType(RecipientListType.Manual)
  }

  def findSmartList(clientId: ObjectId, listType: RecipientListType.Value): Box[RecipientList] = {
    RecipientList where (_.clientId eqs clientId) and (_.listType eqs listType) get()
  }

  def createSmartList(user: User, client: Client, listType: RecipientListType.Value): RecipientList = {
    createInstance(user, client).editable(false).listType(listType).name(RecipientListType.localized(listType)).save(true)
  }

  def findOrCreateSmartList(user: User, client: Client, listType: RecipientListType.Value): RecipientList =
    findSmartList(client.id.get, listType).openOr(createSmartList(user, client, listType))

  def findOrCreateDefaults(user: User, client: Client): List[RecipientList] = {
    findOrCreateSmartList(user, client, RecipientListType.Private) ::
    findOrCreateSmartList(user, client, RecipientListType.All) ::
    findOrCreateSmartList(user, client, RecipientListType.Person) :: Nil
  }

  def findPrivateList(clientId: ObjectId): Box[RecipientList] = findSmartList(clientId, RecipientListType.Private)

  def findClientLists(clientId: ObjectId): List[RecipientList] =
    RecipientList where (_.clientId eqs clientId) orderAsc(_.sortOrder) fetch

  def deleteForClient(clientId: ObjectId) =
    (RecipientList where (_.clientId eqs clientId) fetch) foreach (_.delete_!)

}

class RecipientList private () extends MongoIdRecord[RecipientList] with UserId[RecipientList] with ClientId[RecipientList] with CreatedUpdated[RecipientList] with Loggable {
  def meta = RecipientList

  object editable extends BsBooleanField(this) {
    override def defaultValue = true
  }

  object listType extends BsEnumNameField(this, RecipientListType) {
    override def displayName = "Art der Liste"
    override def defaultValue = RecipientListType.Manual
  }

  object listTypeName extends BsStringField(this, 50) with LifecycleCallbacks {
    override def displayName = "Art der Liste"
    override def defaultValue = RecipientListType.listTypeName(RecipientListType.Manual)

    override def asHtml = Text(S ? this.value)

    override def beforeSave {
      this.set(RecipientListType.listTypeName(listType.get))
    }

  }

  object sortOrder extends BsIntField(this) with LifecycleCallbacks {
    override def defaultValue = 99

    override def beforeSave {
      this.set(RecipientListType.sortOrder(listType.get))
    }

  }

  object name extends BsStringField(this, 255) {
    override def validations = valMinLen(3, "Bitte geben Sie einen Namen an.") _ :: super.validations
    override def displayName = "Name der Liste"
  }

  object recipients extends ObjectIdRefListField(this, Membership) {
    override def displayName = "Verteiler"

    private def namesToShow: String = {
      listType.get match {
        case RecipientListType.Private => "Privat"
        case RecipientListType.All     => "Alle Mitglieder"
        case RecipientListType.Person  => "Einzelperson"
        case RecipientListType.Manual  => objs.take(5).map(_.displayName).mkString(", ") + (if (objs.size > 5) ", ..." else "")
      }
    }

    private def addMemberIds(selected: List[ObjectId], unselected: List[ObjectId]): List[ObjectId] = {
      val cleaned = this.get.filterNot(unselected.contains)
      (cleaned ::: selected).distinct
    }

    private def elem = {
      val id = nextFuncName
      <span>
        <input id={id} type={formInputType} readonly="readonly" class="value form-control input-xlarge"
               value={ namesToShow } style="display:inline-table;" />
        {SHtml.ajaxButton(<i class="icon-plus"></i>, ()=>new Memberships().chooseItem(true, this.get, {
           case SelectedElements(selected, unselected) =>
             this.set(addMemberIds(selected, unselected)); JsCmds.SetValById(id, namesToShow)
           case _ => Noop
          }, "members-items-chooser"), "class"->"btn btn-default value")}
        {SHtml.ajaxButton(<i class="icon-trash"></i>, ()=>{this.set(Nil); JsCmds.SetValById(id, "")}, "class"->"btn btn-danger value")}
      </span>
    }

    override def toForm =
      uniqueFieldId match {
        case Full(id) => Full(elem % ("id" -> id))
        case _ => Full(elem)
      }

    override def asHtml = Text(namesToShow)

  }

  def displayName = name.get

  def memberIdsToUserIds(members: List[ObjectId]): List[ObjectId] = {
    Membership where (_.id in members) select (_.userId) fetch
  }

  def recipientsToUserIds(authorId: ObjectId, oneRecipient: Box[Membership]): List[ObjectId] = listType.get match {
    case RecipientListType.Private => List(authorId)
    case RecipientListType.All => Client.findAllUsers(clientId.get)
    case RecipientListType.Person => oneRecipient.map(_.userId.get).toList
    case RecipientListType.Manual => (authorId :: memberIdsToUserIds(recipients.get)).distinct
  }

  def userIsRecipient(currentUser: User, authorId: ObjectId, oneRecipient: Box[Membership]): Boolean = {
    recipientsToUserIds(authorId, oneRecipient).exists(_ == currentUser.id.get)
  }

  private def usersToRecipientData(userIds: List[ObjectId]): List[RecipientData] = {
    membersToRecipientData(usersToMemberships(userIds))
  }

  private def usersToMemberships(userIds: List[ObjectId]): List[Membership] = {
    userIds.flatMap(id => Membership.findConnection(id, clientId.get))
  }

  private def membersToRecipientData(members: List[Membership]): List[RecipientData] = {
    (for (member <- members) yield {
      var name = member.displayName
      var email = member.userEmail.get.getOrElse("")
      if (name.isEmpty()) name = email
      if (email.isEmpty()) Empty else Full(RecipientData(name, email, member.userId.get))
    }).flatten
  }

  def findEmailRecipients(authorId: ObjectId, recipientData: Recipients[_]): List[RecipientData] = listType.get match {
    case RecipientListType.Private => usersToRecipientData(List(authorId))
    case RecipientListType.All     => membersToRecipientData(Client.findAllMembers(clientId.get))
    case RecipientListType.Person  => membersToRecipientData(recipientData.oneRecipient.obj.toList)
    case RecipientListType.Manual  => (usersToRecipientData(List(authorId)) ::: membersToRecipientData(recipients.objs)).distinct
  }

  def findRecipientMembers(authorId: ObjectId, recipientData: Recipients[_]): List[Membership] = listType.get match {
    case RecipientListType.Private => usersToMemberships(List(authorId))
    case RecipientListType.All     => Client.findAllMembers(clientId.get)
    case RecipientListType.Person  => recipientData.oneRecipient.obj.toList
    case RecipientListType.Manual  => (usersToMemberships(List(authorId)) ::: recipients.objs).distinct
  }

}