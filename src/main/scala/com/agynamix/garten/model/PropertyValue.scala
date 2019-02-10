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
import com.agynamix.garten.model.share.Attachments
import com.agynamix.garten.lib.field.RecipientField
import com.agynamix.garten.model.share.Recipients
import com.agynamix.garten.lib.field.BsObjectIdRefField
import com.agynamix.garten.lib.field.BsStringField
import com.agynamix.garten.lib.field.BsEnumNameField
import com.agynamix.garten.lib.field.BsHtmlAreaField
import com.agynamix.garten.lib.field.OptionalBsLongField
import com.agynamix.garten.lib.field.OptionalBsMoneyField
import com.agynamix.garten.lib.field.OptionalBsStringField
import com.agynamix.garten.lib.field.BsTextareaField
import com.agynamix.garten.lib.util.LocalizedEnum
import com.agynamix.garten.lib.field.BsEnumNameField
import com.agynamix.garten.lib.field.OptionalBsLongField
import com.agynamix.garten.lib.field.OptionalBsMoneyField
import net.liftweb.record.field.OptionalStringField
import net.liftweb.util.FieldError
import com.agynamix.garten.lib.field.OptionalBsDoubleField
import org.joda.money.Money

object PropertyValueType extends Enumeration with LocalizedEnum {
  type PropertyValueType = Value

  val locKeyBase = "property.value.type"

  val NumberType        = Value("number")
  val DoubleType        = Value("double")
  val MoneyType         = Value("money")
  val StringType        = Value("string")
}

object PropertyValue extends PropertyValue with MongoMetaRecord[PropertyValue] with StructuredDbObject[PropertyValue] with FormValidators[PropertyValue] {

  def keyFields(user: User, args: Any*): Box[() => JObject] =
    user.activeMembership.activeClient.map(client => () => (clientId.name -> client.id.get))

  def createInstance(user: User, client: Client) = {
    PropertyValue.createRecord.userId(user.id.get).clientId(client.id.get)
  }

  def findMoneyProperty(clientId: ObjectId, propertyName: String, defValue: Money): Money = {
    (PropertyValue where (_.clientId eqs clientId) and (_.property eqs propertyName) get()) match {
      case Some(property) if property.propertyType.get == PropertyValueType.MoneyType => property.moneyValue.get
      case _ => defValue
    }

  }

}

class PropertyValue private () extends MongoIdRecord[PropertyValue] with ClientId[PropertyValue]
                      with UserId[PropertyValue] with CreatedUpdated[PropertyValue] with Loggable {
  def meta = PropertyValue

  object property extends BsStringField(this, 200) {
    override def displayName = "Schlüssel"
  }

  object propertyType extends BsEnumNameField(this, PropertyValueType) {
    override def displayName = "Typ des Wertes"
    override def defaultValue = PropertyValueType.MoneyType

    override def buildDisplayList = PropertyValueType.buildDisplayList(enum.values.toList)
    override def asHtml = PropertyValueType.asHtml(get)
  }

  object numberValue extends OptionalBsLongField(this) {
    override def asHtml = Text(valueBox.map(_.toString) openOr "")
  }

  object doubleValue extends OptionalBsDoubleField(this) {
    override def asHtml = Text(valueBox.map(_.toString) openOr "")
  }

  object moneyValue extends OptionalBsMoneyField(this)

  object stringValue extends OptionalBsStringField(this, 200) with LifecycleCallbacks {
    override def displayName = "Wert"

    override def validations = valueMustMatchType _ :: super.validations

    def valueMustMatchType(valueToValidate: Option[String]): List[FieldError] = {
      valueToValidate.map(value => {
        propertyType.get match {
          case PropertyValueType.NumberType =>
            asLong(value).map(v => Nil: List[FieldError]).
            openOr(List(FieldError(stringValue, Text("Der angegebene Wert ist keine ganze Zahl"))))
          case PropertyValueType.DoubleType =>
            val kommaToPt = value.replace(",", ".")
            asDouble(kommaToPt).map(v => Nil: List[FieldError]).
            openOr(List(FieldError(stringValue, Text("Der angegebene Wert ist keine Kommazahl"))))
          case PropertyValueType.MoneyType => Nil
          case PropertyValueType.StringType => Nil
        }
      }).getOrElse(List(FieldError(stringValue, Text("Bitte geben Sie einen Wert an."))))
    }

    override def beforeSave {
      numberValue.set(None)
      doubleValue.set(None)
      moneyValue.setBox(Empty)
      valueBox.foreach(value => {
        propertyType.get match {
          case PropertyValueType.NumberType => numberValue.set(asLong(value))
          case PropertyValueType.DoubleType =>
            val kommaToPt = value.replace(",", ".")
            doubleValue.set(asDouble(kommaToPt))
          case PropertyValueType.MoneyType  => moneyValue.setBox(moneyValue.display2money(value))
          case _ =>
        }
      })
    }

    override def asHtml = {
      propertyType.get match {
        case PropertyValueType.NumberType => numberValue.asHtml
        case PropertyValueType.DoubleType => doubleValue.asHtml
        case PropertyValueType.MoneyType  => moneyValue.asHtml
        case _ => super.asHtml
      }
    }

  }


  object note extends BsTextareaField(this, 1000) {
    override def displayName = "Notiz"
    override def textareaRows = 3
  }

}
