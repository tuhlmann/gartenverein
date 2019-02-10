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
import net.liftweb.json._
import com.agynamix.garten.model.share.Schedulable
import net.liftweb.mongodb.record.field.DateField
import net.liftweb.mongodb.record.field.OptionalDateField
import scala.xml.Unparsed
import com.agynamix.garten.lib.field.BsOptionalDateTimeField
import com.agynamix.garten.snippet.TaskModalDialog
import net.liftweb.util.PCDataXmlParser
import com.agynamix.garten.lib.HtmlCleaner
import com.agynamix.garten.snippet.BlogModalDialog
import com.agynamix.garten.model.share.Attachments
import com.agynamix.garten.lib.field.BsObjectIdRefField
import com.agynamix.garten.lib.field.BsStringField
import com.agynamix.garten.lib.field.BsTextareaField
import com.agynamix.garten.lib.field.BsHtmlAreaField

object BlogEntry extends BlogEntry with MongoMetaRecord[BlogEntry] with StructuredDbObject[BlogEntry] with FormValidators[BlogEntry] {

  ensureIndex((snakifiedSubject.name -> 1), true)

  def keyFields(user: User, args: Any*): Box[() => JObject] = Full( () => JObject(List()) )

  def createInstance(user: User, client: Client) = {
    BlogEntry.createRecord.author(user.id.get).clientId(client.id.get)
  }


  def findBySnakeSubj(subj: String) = BlogEntry where (_.snakifiedSubject eqs subj) get()

  def findLatest(count: Int): List[BlogEntry] = {
    BlogEntry orderDesc(_.createdAt) limit(count) fetch
  }

  def url(entry: BlogEntry): String = s"/blog/${entry.snakifiedSubject.get}"

}

class BlogEntry private () extends MongoIdRecord[BlogEntry] with ClientId[BlogEntry]
                with Attachments[BlogEntry] with CreatedUpdated[BlogEntry] with Loggable {
  def meta = BlogEntry

  def getClientId: ObjectId = this.clientId.get
  def uploadDoneCallback = BlogModalDialog.uploadDoneCallback

  object author extends BsObjectIdRefField(this, User) {
    override def displayName = "Autor"
    override def asHtml = Text(obj.map(_.displayName).openOr(""))
  }

  object subject extends BsStringField(this, 255) {
    override def validations = valMinLen(3, "Bitte geben Sie einen Betreff an.") _ :: super.validations
    override def displayName = "Betreff"
  }

  object snakifiedSubject extends BsStringField(this, 255) with LifecycleCallbacks {

    def uniqueSnakeSubj: String = {
      val s = owner.subject.get.replaceAll(" ", "-").toLowerCase()
      (BlogEntry where (_.snakifiedSubject eqs s) and (_.id neqs owner.id.get) get()) match {
        case Some(otherEntry) => s + randomString(3)
        case _ => s
      }
    }

    override def beforeSave {
      this.set(uniqueSnakeSubj)
    }

  }

  object entry extends BsHtmlAreaField(this, 100000) {
    override def displayName = "Text"
  }

}