package com.agynamix.garten.model.share

import net.liftweb.common._
import net.liftweb.util.Helpers._
import net.liftweb.mongodb.record.field.ObjectIdRefField
import net.liftweb.mongodb.record.field.ObjectIdPk
import com.agynamix.garten.lib.StructuredDbObject
import net.liftweb.json.JsonAST._
import net.liftweb.json.JsonDSL._
import java.text.DateFormat
import java.text.SimpleDateFormat
import java.util.Date
import scala.xml.NodeSeq
import net.liftweb.util.BaseField
import net.liftweb.http.S
import scala.xml.Elem
import net.liftweb.json.DefaultFormats
import net.liftweb.record.Field
import net.liftweb.util.FieldError
import net.liftweb.record.OwnedField
import scala.xml.Text
import net.liftweb.record.field.DateTimeTypedField
import java.util.Calendar
import net.liftweb.mongodb.record.MongoRecord
import net.liftweb.record.LifecycleCallbacks
import com.agynamix.garten.model._
import net.liftweb.mongodb.record.BsonRecord
import net.liftweb.mongodb.record.MongoMetaRecord
import net.liftweb.mongodb.record.field.ObjectIdRefListField
import net.liftweb.mongodb.record.field.DateField
import net.liftweb.mongodb.record.field.DateTypedField
import net.liftweb.http.js.JsCmds._
import com.agynamix.garten.lib.field.DateFormAdapterField
import com.agynamix.garten.lib.field.GermanDateField
import net.liftweb.http.js.JsCmd
import org.bson.types.ObjectId
import com.agynamix.garten.lib.field.RecipientField
import net.liftweb.http.SHtml
import java.net.URL
import net.liftweb.http.FileParamHolder
import com.agynamix.garten.lib.field.BsObjectIdRefField
import com.agynamix.garten.snippet.{DbField, Memberships, SelectedElements}
import net.liftweb.http.js.jquery.JqJsCmds.Show
import net.liftweb.http.js.jquery.JqJsCmds.Hide
import com.agynamix.garten.lib.field.BsObjectIdRefListField
import com.agynamix.garten.lib.field.BsEnumNameField
import com.agynamix.garten.lib.field.BsOptionalDateTimeField
import com.mongodb.WriteConcern
import net.liftweb.json.JsonAST.JString
import net.liftweb.http.js.JsCmds.Run
import net.liftweb.http.js.JsCmds.SetValById
import net.liftweb.common.Full
import net.liftweb.json.JsonAST.JInt

object Implicits {
}

  trait UserId[OwnerType <: UserId[OwnerType]] extends BsonRecord[OwnerType] {
    self: OwnerType =>
    def userIdDisplayName = "Nutzer"
    object userId extends ObjectIdRefField(this, User) {
      override def displayName = self.userIdDisplayName
      override def asHtml = Text(obj.map(_.displayName).openOr(""))
      override def asString = obj.map(_.displayName).openOr("")
    }
  }

  trait ClientId[OwnerType <: ClientId[OwnerType]] extends BsonRecord[OwnerType] {
    self: OwnerType =>
    object clientId extends ObjectIdRefField(this, Client)
  }

  trait Attachments[OwnerType <: Attachments[OwnerType]] extends MongoIdRecord[OwnerType]
        with Loggable {
    self: OwnerType =>
    def uploadDoneCallback: JsCmd
    def getClientId: ObjectId
    def isDocumentHidden: Boolean = true
    def maxAttachments = 10
    def removeMarkedDocuments(): Unit = {
      val toRemove = documentsMarkedForRemoval
      if (toRemove.nonEmpty) {
        toRemove.foreach(doc => Document.documentService.remove(doc.relativeFilePath))
        attachments(attachments.objs.filterNot(isDocumentMarkedForRemoval).map(_.id.get))
      }
    }
    def documentsMarkedForRemoval(): List[Document] = { attachments.objs.filter(isDocumentMarkedForRemoval) }
    def markDocumentForRemoval(doc: Document): Unit = {
      markForRemoval.set((doc.id.get :: markForRemoval.get).distinct)
    }

    def isDocumentMarkedForRemoval(doc: Document): Boolean = markForRemoval.get.exists(doc.id.get == _)

    def mimeTypeAllowed(fph: FileParamHolder): Option[String] = attachments.mimeTypeAllowed(fph)

    object attachments extends ObjectIdRefListField(this, Document) with DocumentForm[OwnerType] {
      override def displayName = if (maxAttachments < 2) "Anhang" else "Anhänge"
      def uploadDoneCallback = self.uploadDoneCallback
      def maxAttachments = self.maxAttachments
      def getClientId = self.getClientId
      def isDocumentHidden = self.isDocumentHidden
      def markDocumentForRemoval(doc: Document) = self.markDocumentForRemoval(doc)
      def isDocumentMarkedForRemoval(doc: Document) = self.isDocumentMarkedForRemoval(doc)
    }

    object markForRemoval extends ObjectIdRefListField(this, Document) {
      override def ignoreField_? = true
    }

    override def delete_!(): Boolean = {
      logger.info("Deleting Record, delete Attachments first")
      Document.removeDocuments(attachments.get)
      super.delete_!
    }

  }

  trait Recipients[OwnerType <: Recipients[OwnerType]] extends BsonRecord[OwnerType] with ClientId[OwnerType] {
    self: OwnerType =>

    def recipientsDisplay: NodeSeq = {
      if (recipients.isPersonType) {
        oneRecipient.recipientAsHtml
      } else {
        recipients.recipientsAsHtml
      }
    }

    def recipientsDisplayname = "Verteiler"

    /**
     * Is this user a recipient of this element, can he view it.
     */
    def userIsRecipient(currentUser: User, authorId: ObjectId): Boolean =
      recipients.obj.map(_.userIsRecipient(currentUser, authorId, oneRecipient.obj)) openOr false

    object recipients extends BsObjectIdRefField(this, RecipientList) {
      override def displayName = recipientsDisplayname

      def isPersonType = obj.map(_.listType.get) === RecipientListType.Person

      override def defaultValueBox = {
        val allLists = RecipientList.findClientLists(clientId.get)
        Box((allLists.find(_.listType.get == RecipientListType.Private).orElse(allLists.headOption)).map(_.id.get))
      }

      override def asHtml = recipientsDisplay //Text(obj.map(_.name.get) openOr "")
      def recipientsAsHtml = Text(obj.map(_.name.get) openOr "")

      override def options: List[(Box[ObjectId], String)] = {
        RecipientList.findClientLists(clientId.get) map (r => (Full(r.id.get), r.displayName))
      }
      override def toForm: Box[NodeSeq] = {
        val opt = buildDisplayList.flatMap(r => r._1.map((_, r._2)))
        Full(SHtml.ajaxSelectObj[ObjectId](opt, obj.map(_.id.get), (v: ObjectId) => {
          this.set(v)
          if (isPersonType) {
            Show(oneRecipientId)
          } else {
            oneRecipient.clearField &
            Hide(oneRecipientId)
          }
        }, "class" -> "form-control"))
      }
    }

    val oneRecipientId = nextFuncName

    object oneRecipient extends OptionalObjectIdRefField(this, Membership) {
      override def displayName = "Einzelperson"

      lazy val itsFieldId: String = uniqueFieldId.openOr(randomString(12))

      def clearField: JsCmd = {
        setBox(Empty)
        SetValById(itsFieldId, "")
      }

      private def namesToShow: String = obj.map(_.displayName).openOr("")
      private def elem = {
        <span>
          <input id={itsFieldId} type={formInputType} readonly="readonly" class="value form-control input-xlarge"
                 value={ namesToShow } style="display:inline-table;" />
          <span id={oneRecipientId} style={if (recipients.isPersonType) "" else "display:none;"}>
          {SHtml.ajaxButton(<i class="icon-plus"></i>, ()=>new Memberships().chooseItem(false, Nil, {
             case SelectedElements(selected, unselected) => this.setBox(selected.headOption); SetValById(itsFieldId, namesToShow)
             case _ => Noop
            }, "members-items-chooser"), "class"->"btn btn-default value")}
          {SHtml.ajaxButton(<i class="icon-trash"></i>, ()=>{this.setBox(Empty); SetValById(itsFieldId, "")}, "class"->"btn btn-danger value")}
          </span>
        </span>
      }
      override def toForm: Box[NodeSeq] = Full(elem)

      override def asHtml = recipientsDisplay
      def recipientAsHtml = Text(namesToShow)

    }

  }

  trait GeneratedDocumentsHolder[OwnerType <: GeneratedDocumentsHolder[OwnerType]] extends BsonRecord[OwnerType] with ClientId[OwnerType] {
    self: OwnerType =>

    object documents extends BsObjectIdRefListField(this, Document) {
      override def displayName = "Dokumente"

      override def toForm = Full(asHtml)
      override def asHtml = {
        <div style="max-height:600px; overflow:auto;">
          <table class="table table-bordered table-striped table-hover table-condensed">
            {objs.map(d => {
              <tr><td>{d.userId.obj.map(_.fancyEmail).openOr("")}</td><td>{downloadBtn(d)}</td></tr>
            })
            }
          </table>
        </div>
      }
    }

    private def downloadBtn(document: Document) = {
      <button class="btn btn-info btn-xs" onclick={SHtml.ajaxInvoke(()=>{
        Run("window.open('%s', '_newtab')".format(document.downloadDocumentUrl()))
        })._2.toJsCmd}><i class="icon-download-alt"></i>&nbsp;Download
      </button>
    }

    object zipAllDocuments extends BsObjectIdRefField(this, Document) {
      override def displayName = "Alle Dokumente"
      override def toForm = Full(asHtml)
      override def optional_? = true

      override def asHtml = obj match {
        case Full(document) => <div>{downloadBtn(document)}</div>
        case _ => NodeSeq.Empty
      }
    }

    object zipUsersWithoutEmailDocuments extends BsObjectIdRefField(this, Document) {
      override def displayName = "Ohne Email"
      override def toForm = Full(asHtml)
      override def optional_? = true

      override def asHtml = obj match {
        case Full(document) => <div>{downloadBtn(document)}</div>
        case _ => NodeSeq.Empty
      }
    }

    object generationStatus extends BsEnumNameField(this, GenerationStatus) {
      override def displayName = "Status der Generierung"
      override def buildDisplayList = GenerationStatus.buildDisplayList(enum.values.toList)
      override def asHtml = GenerationStatus.asHtml(get)
      override def toForm = Full(<p class="form-control-static">{asHtml}</p>)
    }

    object lastGenerated extends BsOptionalDateTimeField(this) {
      override def displayName = "Zuletzt generiert"
      override def toForm = Full(<p class="form-control-static">{asHtml}</p>)
    }

  }

  trait TemplateProvider {
    def templateUrl: Box[URL]
    def getSubject(): String
  }

  abstract class OptionalObjectIdRefField[OwnerType <: BsonRecord[OwnerType], RefType <: MongoRecord[RefType]](rec: OwnerType,
      val itsMeta: MongoMetaRecord[RefType]) extends ObjectIdRefField(rec, itsMeta) {

    override def optional_? = true

  }

  abstract class OptionalObjectIdRefListField[OwnerType <: BsonRecord[OwnerType], RefType <: MongoRecord[RefType]](rec: OwnerType,
      val itsMeta: MongoMetaRecord[RefType]) extends ObjectIdRefListField(rec, itsMeta) {

    override def optional_? = true

  }

  trait MongoIdRecord[BaseRecord <: MongoIdRecord[BaseRecord] with ObjectIdPk[BaseRecord]] extends MongoRecord[BaseRecord] with ObjectIdPk[BaseRecord] {
    self: BaseRecord =>

    def allRecordFields: Seq[BaseField] = allFields

    def setSelected(b: Boolean): Unit = {}
    def isSelected: Boolean = false

    def getField(field: String) = allRecordFields.find(_.name == field)

    def filterMatches(filterFields: List[DbField], query: String)(implicit m: Manifest[BaseRecord]): Boolean = {
      val ql = query.trim.toLowerCase()

      filterFields.exists(filterField => {
        filterField.fieldValue(this).toString().toLowerCase().indexOf(ql) > -1
      })
    }



  }

  trait MongoIdMetaRecord[BaseRecord <: MongoIdRecord[BaseRecord]] extends MongoMetaRecord[BaseRecord] with LogbookMetaRecord[BaseRecord] {
    self: BaseRecord =>

  }

  trait LogbookMetaRecord[BaseRecord <: MongoIdRecord[BaseRecord]] extends MongoMetaRecord[BaseRecord] {
    self: BaseRecord =>


    override def save(inst: BaseRecord, concern: WriteConcern): Boolean = {
      if (inst.id.value.isNew) {
        //println(s"New record: ${inst.id.get}")
        Logbook.logCreateRecord(inst)
      } else {
        //println(s"Not a new Record: ${inst.id.get}")
        Logbook.logUpdateRecord(inst)
      }

      super.save(inst, concern)
    }

    override def delete_!(inst: BaseRecord): Boolean = {
      Logbook.logDeleteRecord(inst)
      super.delete_!(inst)
    }


  }

  trait Created[OwnerType <: Created[OwnerType]] extends BsonRecord[OwnerType] {
    self: OwnerType =>

    object createdAt extends DateField(this) with DateFormAdapterField with GermanDateField {
      override def displayName = "Hinzugefügt"
    }

  }

  trait CreatedUpdated[OwnerType <: CreatedUpdated[OwnerType]] extends BsonRecord[OwnerType] {
    self: OwnerType =>

    var updateUpdated = true

    def ignoreUpdatedDate = {
      updateUpdated = false
      this
    }

    object createdAt extends DateField(this) with DateFormAdapterField with GermanDateField {
      override def displayName = "Hinzugefügt"
    }

    object updatedAt extends DateField(this) with DateFormAdapterField with GermanDateField with LifecycleCallbacks {
      override def displayName = "Bearbeitet"
      override def beforeSave() = if (updateUpdated) this(new Date())
    }
  }

  trait Selectable[OwnerType <: Selectable[OwnerType]] extends MongoIdRecord[OwnerType] {
    self: OwnerType =>

    object selected extends ElementSelectedField(this)

    override def setSelected(b: Boolean): Unit = selected(b)
    override def isSelected: Boolean = selected.get
  }


  trait UniqueFieldId extends BaseField {
    self: BaseField =>

    lazy val _uniqueFieldId = randomString(12)
    override def uniqueFieldId = Full(_uniqueFieldId)
  }


  trait FormValidators[OwnerType <: MongoIdRecord[OwnerType]] extends StructuredDbObject[OwnerType] {

    self: OwnerType =>

    implicit def str2jv[FieldType](in: FieldType): JValue = in match {
      case in: Int => JInt(in)
      case in      => JString(in.toString)
    }

    def convertIntoAny[RefFieldType](in: Any)(implicit m: Manifest[RefFieldType]): Any = in match {
      case value       if m.erasure.isInstance(Int)   => asInt(value.toString).openOr(0)
      case value       if m.erasure.isInstance(Long)  => asLong(value.toString).openOr(0)
      case value       if m.erasure.isInstance(value) => value
      case value                                      => value.toString
    }

    def uniqueValue[FieldType](field: Field[FieldType, OwnerType] with OwnedField[OwnerType], msg: => String)(value: FieldType): List[FieldError] = {

      (for (user <- User.currentUser; itsKeyFields <- keyFields(user)) yield {
        val query = itsKeyFields() ~ ("$and" -> List((field.name -> value) ~ (field.owner.id.name -> ("$ne" -> field.owner.id.get))))
        meta.find(query) match {
          case Full(r) => List(FieldError(field, Text(msg)))
          case _       => Nil
        }
      }) openOr Nil
    }

    def valueExists[FieldType, RefFieldType](myField: Field[FieldType, _], refField: Field[RefFieldType, OwnerType], msg: => String)(value: FieldType): List[FieldError] = {

      (for (user <- User.currentUser; itsKeyFields <- keyFields(user)) yield {
        val query = itsKeyFields() ~ ("$and" -> List(refField.name -> convertIntoAny(value)))

        meta.find(query) match {
          case Full(r) => Nil
          case _       => List(FieldError(myField, Text(msg)))
        }
      }) openOr Nil
    }

    def nextIntValue[FieldType](field: Field[FieldType, OwnerType] with OwnedField[OwnerType], defaultValue: Int): Int = {

      (for (user <- User.currentUser; itsKeyFields <- keyFields(user)) yield {
        val query = itsKeyFields()
        meta.findAll(query, "member_no" -> -1) match {
          case rec :: _       => rec.fieldByName(field.name).flatMap(f => tryo(f.get.toString().toInt+1)).openOr(defaultValue)
          case _ => defaultValue
        }
      }) openOr defaultValue
    }

  }
